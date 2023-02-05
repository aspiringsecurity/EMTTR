import {
    TASK_COMPILE_SOLIDITY_RUN_SOLC,
    TASK_COMPILE_SOLIDITY_GET_ARTIFACT_FROM_COMPILATION_OUTPUT,
    TASK_COMPILE_SOLIDITY_GET_SOLC_BUILD,
    TASK_COMPILE_SOLIDITY_GET_COMPILATION_JOBS,
    TASK_COMPILE_SOLIDITY_LOG_COMPILATION_RESULT,
} from 'hardhat/builtin-tasks/task-names';
import { extendEnvironment, extendConfig, subtask } from 'hardhat/internal/core/config/config-env';
import './type-extensions';
import { FactoryDeps } from './types';
import { Artifacts, getArtifactFromContractOutput } from 'hardhat/internal/artifacts';
import { compile } from './compile';
import {
    zeroxlify,
    getZksolcPath,
    getZksolcUrl,
    filterSupportedOutputSelections,
    pluralize,
    getVersionComponents,
    isURL,
    saltFromUrl,
} from './utils';
import { spawnSync } from 'child_process';
import { download } from 'hardhat/internal/util/download';
import fs from 'fs';
import chalk from 'chalk';
import { defaultZkSolcConfig, ZKSOLC_BIN_REPOSITORY, ZK_ARTIFACT_FORMAT_VERSION } from './constants';
import { ZkSyncSolcPluginError } from './errors';
import { CompilationJob } from 'hardhat/types';

extendConfig((config, userConfig) => {
    if (userConfig?.zksolc?.settings?.optimizer) {
        console.warn('`optimizer` setting is deprecated, optimizer is always enabled');
    }

    config.zksolc = { ...defaultZkSolcConfig, ...userConfig?.zksolc };
    config.zksolc.settings = { ...defaultZkSolcConfig.settings, ...userConfig?.zksolc?.settings };
});

extendEnvironment((hre) => {
    if (hre.network.config.zksync) {
        hre.network.zksync = hre.network.config.zksync;

        let artifactsPath = hre.config.paths.artifacts;
        if (!artifactsPath.endsWith('-zk')) {
            artifactsPath = artifactsPath + '-zk';
        }

        let cachePath = hre.config.paths.cache;
        if (!cachePath.endsWith('-zk')) {
            cachePath = cachePath + '-zk';
        }

        // Forcibly update the artifacts object.
        hre.config.paths.artifacts = artifactsPath;
        hre.config.paths.cache = cachePath;
        (hre as any).artifacts = new Artifacts(artifactsPath);

        hre.config.solidity.compilers.forEach((compiler) => {
            const [major, minor] = getVersionComponents(compiler.version);
            if (major === 0 && minor < 8 && hre.config.zksolc.settings.forceEvmla) {
                console.warn('zksolc solidity compiler versions < 0.8 work with forceEvmla enabled by default');
            }
            let settings = compiler.settings || {};
            // If solidity optimizer is not enabled, the libraries are not inlined and
            // we have to manually pass them into zksolc. That's why we force the optimization.
            compiler.settings = { ...settings, optimizer: { enabled: true } };

            // zkSolc supports only a subset of solc output selections
            compiler.settings.outputSelection = filterSupportedOutputSelections(compiler.settings.outputSelection);
        });
    }
});

// This override is needed to invalidate cache when zksolc config is changed.
subtask(TASK_COMPILE_SOLIDITY_GET_COMPILATION_JOBS, async (args, hre, runSuper) => {
    const { jobs, errors } = await runSuper(args);
    jobs.forEach((job: any) => {
        job.solidityConfig.zksolc = hre.config.zksolc;
    });
    return { jobs, errors };
});

subtask(
    TASK_COMPILE_SOLIDITY_GET_ARTIFACT_FROM_COMPILATION_OUTPUT,
    async (
        {
            sourceName,
            contractName,
            contractOutput,
        }: {
            sourceName: string;
            contractName: string;
            contractOutput: any;
        },
        hre
    ): Promise<any> => {
        if (hre.network.zksync !== true) {
            return getArtifactFromContractOutput(sourceName, contractName, contractOutput);
        }
        let bytecode: string =
            contractOutput.evm?.bytecode?.object || contractOutput.evm?.deployedBytecode?.object || '';
        bytecode = zeroxlify(bytecode);

        let factoryDeps: FactoryDeps = {};
        let entries: Array<[string, string]> = Object.entries(contractOutput.factoryDependencies || {});
        for (const [hash, dependency] of entries) {
            factoryDeps[zeroxlify(hash)] = dependency;
        }

        return {
            _format: ZK_ARTIFACT_FORMAT_VERSION,
            contractName,
            sourceName,
            abi: contractOutput.abi,
            // technically, zkEVM has no difference between bytecode & deployedBytecode,
            // but both fields are included just in case
            bytecode,
            deployedBytecode: bytecode,
            // zksolc does not support unlinked objects,
            // all external libraries are either linked during compilation or inlined
            linkReferences: {},
            deployedLinkReferences: {},

            // zkSync-specific field
            factoryDeps,
        };
    }
);

subtask(TASK_COMPILE_SOLIDITY_RUN_SOLC, async (args: { input: any; solcPath: string }, hre, runSuper) => {
    if (hre.network.zksync !== true) {
        return await runSuper(args);
    }

    if (hre.config.zksolc.settings.libraries) {
        args.input.settings.libraries = hre.config.zksolc.settings.libraries;
    }

    return await compile(hre.config.zksolc, args.input, args.solcPath);
});

// This task is overriden to:
// - prevent unnecessary solc downloads when using docker
// - download zksolc binary if needed
// - validate zksolc binary
subtask(TASK_COMPILE_SOLIDITY_GET_SOLC_BUILD, async (args: { solcVersion: string }, hre, runSuper) => {
    if (hre.network.zksync !== true) {
        return await runSuper(args);
    }

    if (hre.config.zksolc.compilerSource === 'docker') {
        // Versions are wrong here when using docker, because there is no
        // way to know them beforehand except to run the docker image, which
        // adds 5-10 seconds to startup time. We cannot read them from artifacts,
        // since that would make cache invalid every time, if the version is
        // different from the one in the docker image.
        //
        // If you wish to know the actual versions from build-info files,
        // please look at `output.version`, `output.long_version`
        // and `output.zk_version` in the generated JSON.
        return {
            compilerPath: '',
            isSolsJs: false,
            version: args.solcVersion,
            longVersion: '',
        };
    }

    const solcBuild = await runSuper(args);
    let compilerPath = hre.config.zksolc.settings.compilerPath;

    if (compilerPath) {
        if (isURL(compilerPath)) {
            const compilerUrl = compilerPath;
            // hashed url used as a salt to avoid name collisions
            const salt = saltFromUrl(compilerUrl);
            // where the binary will be downloaded to
            hre.config.zksolc.settings.compilerPath = compilerPath = await getZksolcPath(
                hre.config.zksolc.version,
                salt
            );
            if(!fs.existsSync(compilerPath)) {
                console.info(chalk.yellow(`Downloading zksolc from ${compilerUrl}`));
                try {
                    await download(compilerUrl, compilerPath);
                    console.info(chalk.green(`zksolc successfully downloaded`));
                    fs.chmodSync(compilerPath, '755');
                } catch (e: any) {
                    throw new ZkSyncSolcPluginError(e.message.split('\n')[0]);
                }  
            }
        }
        const versionOutput = spawnSync(compilerPath, ['--version']);
        const version = versionOutput.stdout
            ?.toString()
            .match(/\d+\.\d+\.\d+/)
            ?.toString();

        if (versionOutput.status !== 0 || version == null) {
            throw new ZkSyncSolcPluginError(`Specified zksolc binary is not found or invalid`);
        }
    } else {
        compilerPath = await getZksolcPath(hre.config.zksolc.version);
        if (!fs.existsSync(compilerPath)) {
            console.info(chalk.yellow(`Downloading zksolc ${hre.config.zksolc.version}`));
            try {
                await download(getZksolcUrl(ZKSOLC_BIN_REPOSITORY, hre.config.zksolc.version), compilerPath);
                fs.chmodSync(compilerPath, '755');
                console.info(chalk.green(`zksolc version ${hre.config.zksolc.version} successfully downloaded`));
            } catch (e: any) {
                throw new ZkSyncSolcPluginError(e.message.split('\n')[0]);
            }
        }
    }

    return solcBuild;
});

subtask(
    TASK_COMPILE_SOLIDITY_LOG_COMPILATION_RESULT,
    async ({ compilationJobs }: { compilationJobs: CompilationJob[] }) => {
        let count = 0;
        for (const job of compilationJobs) {
            count += job.getResolvedFiles().filter((file) => job.emitsArtifacts(file)).length;
        }

        if (count > 0) {
            console.info(chalk.green(`Successfully compiled ${count} Solidity ${pluralize(count, 'file')}`));
        }
    }
);

export {
    getZksolcPath,
    getZksolcUrl,
    ZKSOLC_BIN_REPOSITORY,
    saltFromUrl
};
