import { AssertionError, expect } from 'chai';
import { BigNumber } from 'ethers';
import * as zk from 'zksync-web3';
import path from 'path';
import util from 'util';

import { Deployer } from '@matterlabs/hardhat-zksync-deploy/src/deployer';
import { ZkSyncArtifact } from '@matterlabs/hardhat-zksync-deploy/src/types';
import { anyUint, anyValue } from '@nomicfoundation/hardhat-chai-matchers/internal/withArgs';

import { runSuccessfulAsserts, runFailedAsserts, useEnvironmentWithLocalSetup } from '../helpers';
import '../../src/internal/add-chai-matchers';

const RICH_WALLET_PK = '0x7726827caac94a7f9e1b160f7ea819f172f7b6f9d2a97f992c38edeab82d4110';

describe('INTEGRATION: Reverted with custom error', function () {
    describe('with the local setup', function () {
        useEnvironmentWithLocalSetup('hardhat-project');

        runTests();
    });

    function runTests() {
        let matchers: zk.Contract;
        let provider: zk.Provider;
        let wallet: zk.Wallet;
        let deployer: Deployer;
        let artifact: ZkSyncArtifact;

        beforeEach('deploy matchers contract', async function () {
            provider = zk.Provider.getDefaultProvider();
            wallet = new zk.Wallet(RICH_WALLET_PK, provider);

            deployer = new Deployer(this.hre, wallet);
            artifact = await deployer.loadArtifact('Matchers');
            matchers = await deployer.deploy(artifact);
        });

        describe('calling a method that succeeds', function () {
            it('successful asserts', async function () {
                await runSuccessfulAsserts({
                    matchers,
                    method: 'succeeds',
                    successfulAssert: (x) => expect(x).not.to.be.revertedWithCustomError(matchers, 'SomeCustomError'),
                });
            });

            it('failed asserts', async function () {
                await runFailedAsserts({
                    matchers,
                    method: 'succeeds',
                    failedAssert: (x) => expect(x).to.be.revertedWithCustomError(matchers, 'SomeCustomError'),
                    failedAssertReason:
                        "Expected transaction to be reverted with custom error 'SomeCustomError', but it didn't revert",
                });
            });
        });

        describe('calling a method that reverts without a reason', function () {
            it('successful asserts', async function () {
                await runSuccessfulAsserts({
                    matchers,
                    method: 'revertsWithoutReason',
                    successfulAssert: (x) => expect(x).to.not.be.revertedWithCustomError(matchers, 'SomeCustomError'),
                });
            });
        });

        describe('calling a method that reverts with a reason string', function () {
            it('successful asserts', async function () {
                await runSuccessfulAsserts({
                    matchers,
                    method: 'revertsWith',
                    args: ['some reason'],
                    successfulAssert: (x) => expect(x).to.not.be.revertedWithCustomError(matchers, 'SomeCustomError'),
                });
            });

            it('failed asserts', async function () {
                await runFailedAsserts({
                    matchers,
                    method: 'revertsWith',
                    args: ['some reason'],
                    failedAssert: (x) => expect(x).to.be.revertedWithCustomError(matchers, 'SomeCustomError'),
                    failedAssertReason:
                        "Expected transaction to be reverted with custom error 'SomeCustomError', but it reverted with reason 'some reason'",
                });
            });
        });

        describe('calling a method that reverts with a panic code', function () {
            it('successful asserts', async function () {
                await runSuccessfulAsserts({
                    matchers,
                    method: 'panicAssert',
                    successfulAssert: (x) => expect(x).to.not.be.revertedWithCustomError(matchers, 'SomeCustomError'),
                });
            });

            it('failed asserts', async function () {
                await runFailedAsserts({
                    matchers,
                    method: 'panicAssert',
                    failedAssert: (x) => expect(x).to.be.revertedWithCustomError(matchers, 'SomeCustomError'),
                    failedAssertReason:
                        "Expected transaction to be reverted with custom error 'SomeCustomError', but it reverted with panic code 0x01 (Assertion error)",
                });
            });
        });

        describe('calling a method that reverts with a custom error', function () {
            it('successful asserts', async function () {
                await runSuccessfulAsserts({
                    matchers,
                    method: 'revertWithSomeCustomError',
                    successfulAssert: (x) => expect(x).to.be.revertedWithCustomError(matchers, 'SomeCustomError'),
                });

                await runSuccessfulAsserts({
                    matchers,
                    method: 'revertWithAnotherCustomError',
                    successfulAssert: (x) => expect(x).to.not.be.revertedWithCustomError(matchers, 'SomeCustomError'),
                });
            });

            it('failed asserts: expected custom error not to match', async function () {
                await runFailedAsserts({
                    matchers,
                    method: 'revertWithSomeCustomError',
                    failedAssert: (x) => expect(x).to.not.be.revertedWithCustomError(matchers, 'SomeCustomError'),
                    failedAssertReason:
                        "Expected transaction NOT to be reverted with custom error 'SomeCustomError', but it was",
                });
            });

            it('failed asserts: reverts with another custom error of the same contract', async function () {
                await runFailedAsserts({
                    matchers,
                    method: 'revertWithAnotherCustomError',
                    failedAssert: (x) => expect(x).to.be.revertedWithCustomError(matchers, 'SomeCustomError'),
                    failedAssertReason:
                        "Expected transaction to be reverted with custom error 'SomeCustomError', but it reverted with custom error 'AnotherCustomError'",
                });
            });

            it('failed asserts: reverts with another custom error of another contract', async function () {
                await runFailedAsserts({
                    matchers,
                    method: 'revertWithAnotherContractCustomError',
                    failedAssert: (x) => expect(x).to.be.revertedWithCustomError(matchers, 'SomeCustomError'),
                    failedAssertReason:
                        "Expected transaction to be reverted with custom error 'SomeCustomError', but it reverted with a different custom error",
                });
            });
        });

        describe('with args', function () {
            it('should work with one argument', async function () {
                await expect(matchers.revertWithCustomErrorWithUint(1))
                    .to.be.revertedWithCustomError(matchers, 'CustomErrorWithUint')
                    .withArgs(1);

                await expect(
                    expect(matchers.revertWithCustomErrorWithUint(1))
                        .to.be.revertedWithCustomError(matchers, 'CustomErrorWithUint')
                        .withArgs(2)
                ).to.be.rejectedWith(AssertionError, 'expected 1 to equal 2');
            });

            it('should work with two arguments', async function () {
                await expect(matchers.revertWithCustomErrorWithUintAndString(1, 'foo'))
                    .to.be.revertedWithCustomError(matchers, 'CustomErrorWithUintAndString')
                    .withArgs(1, 'foo');

                await expect(
                    expect(matchers.revertWithCustomErrorWithUintAndString(1, 'foo'))
                        .to.be.revertedWithCustomError(matchers, 'CustomErrorWithUintAndString')
                        .withArgs(2, 'foo')
                ).to.be.rejectedWith(AssertionError, 'expected 1 to equal 2');

                await expect(
                    expect(matchers.revertWithCustomErrorWithUintAndString(1, 'foo'))
                        .to.be.revertedWithCustomError(matchers, 'CustomErrorWithUintAndString')
                        .withArgs(1, 'bar')
                ).to.be.rejectedWith(AssertionError, "expected 'foo' to equal 'bar'");

                await expect(
                    expect(matchers.revertWithCustomErrorWithUintAndString(1, 'foo'))
                        .to.be.revertedWithCustomError(matchers, 'CustomErrorWithUintAndString')
                        .withArgs(() => {
                            throw new Error('user-defined error');
                        }, 'foo')
                ).to.be.rejectedWith(Error, 'user-defined error');
            });

            it('should check the length of the args', async function () {
                await expect(
                    expect(matchers.revertWithCustomErrorWithUintAndString(1, 's'))
                        .to.be.revertedWithCustomError(matchers, 'CustomErrorWithUintAndString')
                        .withArgs(1)
                ).to.be.rejectedWith(AssertionError, 'expected 1 args but got 2');

                await expect(
                    expect(matchers.revertWithCustomErrorWithUintAndString(1, 's'))
                        .to.be.revertedWithCustomError(matchers, 'CustomErrorWithUintAndString')
                        .withArgs(1, 's', 3)
                ).to.be.rejectedWith(AssertionError, 'expected 3 args but got 2');
            });

            it('should work with nested arguments', async function () {
                await expect(matchers.revertWithCustomErrorWithPair(1, 2))
                    .to.be.revertedWithCustomError(matchers, 'CustomErrorWithPair')
                    .withArgs([1, 2]);

                await expect(
                    expect(matchers.revertWithCustomErrorWithPair(1, 2))
                        .to.be.revertedWithCustomError(matchers, 'CustomErrorWithPair')
                        .withArgs([3, 2])
                ).to.be.rejectedWith(AssertionError, /expected \[.*\] to deeply equal \[ 3, 2 \]/s);
            });

            it('Should fail when used with .not.', async function () {
                expect(() =>
                    expect(matchers.revertWithSomeCustomError())
                        .to.not.be.revertedWithCustomError(matchers, 'SomeCustomError')
                        .withArgs(1)
                ).to.throw(Error, 'Do not combine .not. with .withArgs()');
            });

            it('should fail if withArgs is called on its own', async function () {
                expect(() =>
                    expect(matchers.revertWithCustomErrorWithUint(1))
                        // @ts-expect-error
                        .withArgs(1)
                ).to.throw(
                    Error,
                    'withArgs can only be used in combination with a previous .emit or .revertedWithCustomError assertion'
                );
            });

            it('should fail if both emit and revertedWithCustomError are called', async function () {
                expect(() =>
                    expect(matchers.revertWithSomeCustomError())
                        .to.emit(matchers, 'SomeEvent')
                        .and.to.be.revertedWithCustomError(matchers, 'SomeCustomError')
                        .withArgs(1)
                ).to.throw(
                    Error,
                    'withArgs called with both .emit and .revertedWithCustomError, but these assertions cannot be combined'
                );
            });

            it('should work with bigints and bignumbers', async function () {
                await expect(matchers.revertWithCustomErrorWithUint(1))
                    .to.be.revertedWithCustomError(matchers, 'CustomErrorWithUint')
                    .withArgs(BigInt(1));

                await expect(matchers.revertWithCustomErrorWithUint(1))
                    .to.be.revertedWithCustomError(matchers, 'CustomErrorWithUint')
                    .withArgs(BigNumber.from(1));

                await expect(matchers.revertWithCustomErrorWithPair(1, 2))
                    .to.be.revertedWithCustomError(matchers, 'CustomErrorWithPair')
                    .withArgs([BigInt(1), BigNumber.from(2)]);
            });

            it('should work with predicates', async function () {
                await expect(matchers.revertWithCustomErrorWithUint(1))
                    .to.be.revertedWithCustomError(matchers, 'CustomErrorWithUint')
                    .withArgs(anyValue);

                await expect(
                    expect(matchers.revertWithCustomErrorWithUint(1))
                        .to.be.revertedWithCustomError(matchers, 'CustomErrorWithUint')
                        .withArgs(() => false)
                ).to.be.rejectedWith(
                    AssertionError,
                    'The predicate for custom error argument with index 0 returned false'
                );

                await expect(matchers.revertWithCustomErrorWithUint(1))
                    .to.be.revertedWithCustomError(matchers, 'CustomErrorWithUint')
                    .withArgs(anyUint);

                await expect(
                    expect(matchers.revertWithCustomErrorWithInt(-1))
                        .to.be.revertedWithCustomError(matchers, 'CustomErrorWithInt')
                        .withArgs(anyUint)
                ).to.be.rejectedWith(
                    AssertionError,
                    'The predicate for custom error argument with index 0 threw an AssertionError: anyUint expected its argument to be an unsigned integer, but it was negative, with value -1'
                );
            });
        });

        describe('invalid values', function () {
            it('non-errors as subject', async function () {
                await expect(
                    expect(Promise.reject({})).to.be.revertedWithCustomError(matchers, 'SomeCustomError')
                ).to.be.rejectedWith(AssertionError, 'Expected an Error object');
            });

            it('the contract is not specified', async function () {
                expect(() =>
                    expect(matchers.revertWithSomeCustomError())
                        .to.be // @ts-expect-error
                        .revertedWithCustomError('SomeCustomError')
                ).to.throw(
                    TypeError,
                    'The first argument of .revertedWithCustomError must be the contract that defines the custom error'
                );
            });

            it("the contract doesn't have a custom error with that name", async function () {
                expect(() =>
                    expect(matchers.revertWithSomeCustomError()).to.be.revertedWithCustomError(
                        matchers,
                        'SomeCustmError'
                    )
                ).to.throw(Error, "The given contract doesn't have a custom error named 'SomeCustmError'");
            });

            it('errors that are not related to a reverted transaction', async function () {
                const signer = zk.Wallet.createRandom().connect(provider);

                await expect(
                    expect(
                        matchers.connect(signer).revertsWithoutReason({
                            gasLimit: 1_000_000,
                        })
                    ).to.not.be.revertedWithCustomError(matchers, 'SomeCustomError')
                ).to.be.eventually.rejectedWith(
                    Error,
                    'Not enough balance to cover the fee + value.'
                );
            });
        });

        describe('stack traces', function () {
            it('includes test file', async function () {
                try {
                    await expect(matchers.revertedWith('some reason')).to.be.revertedWithCustomError(
                        matchers,
                        'SomeCustomError'
                    );
                } catch (e: any) {
                    expect(util.inspect(e)).to.include(path.join('test', 'reverted', 'revertedWithCustomError.ts'));

                    return;
                }

                expect.fail('Expected an exception but none was thrown');
            });
        });
    }
});
