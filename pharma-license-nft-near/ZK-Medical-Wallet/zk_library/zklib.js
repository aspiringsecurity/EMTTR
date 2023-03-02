const execSync = require('child_process').execSync;
const fs = require("fs");
const BigNumber = require('bignumber.js');
const { exec } = require('child_process');


async function generate_age_proof(age, age_bar){

    let compute_witness_command = 'zokrates compute-witness -a ' + String(age) + ' ' + String(age_bar);
    console.log(compute_witness_command);
    const output1 = execSync(compute_witness_command, {cwd: '/home/akash/Videos/MIT-Blockchain-Hackathon-2020/zk_library/age', encoding: 'utf-8' });

    let generate_proof_command = 'zokrates generate-proof';
    const output2 = execSync(generate_proof_command, {cwd: '/home/akash/Videos/MIT-Blockchain-Hackathon-2020/zk_library/age', encoding: 'utf-8' });
    let proof = require("./age/proof.json");
   
    return proof;
}


async function generate_password_proof(password, password_hash){

    let x1 = BigNumber(password_hash.substr(0,32), 16).toFixed();
    let x2 = BigNumber(password_hash.substr(32,32), 16).toFixed();
    let compute_witness_command = 'zokrates compute-witness -a 0 0 0 ' + String(password) + ' ' + (x1) + ' ' + (x2);
    console.log(compute_witness_command);
    execSync(compute_witness_command, {cwd: '/home/akash/Videos/MIT-Blockchain-Hackathon-2020/zk_library/password', encoding: 'utf-8', maxBuffer:10000*1024 });

    let generate_proof_command = 'zokrates generate-proof';
    execSync(generate_proof_command, {cwd: '/home/akash/Videos/MIT-Blockchain-Hackathon-2020/zk_library/password', encoding: 'utf-8', maxBuffer:10000*1024 });
    let proof = require("./password/proof.json");
   
    return proof;
}

async function generate_vote_proof(secret, x1, x2, x3){

    let compute_witness_command = 'zokrates compute-witness -a ' + String(secret) + ' ' + String(x1) + ' ' + String(x2) + ' ' + String(x3);
    console.log(compute_witness_command);
    execSync(compute_witness_command, {cwd: '/home/akash/Videos/MIT-Blockchain-Hackathon-2020/zk_library/vote', encoding: 'utf-8', maxBuffer:10000*1024 });

    let generate_proof_command = 'zokrates generate-proof';
    execSync(generate_proof_command, {cwd: '/home/akash/Videos/MIT-Blockchain-Hackathon-2020/zk_library/vote', encoding: 'utf-8', maxBuffer:10000*1024 });
    let proof = require("./vote/proof.json");

    return proof;
}

async function get_hash(password){

    let compute_witness_command = 'zokrates compute-witness -a 0 0 0 ' + String(password);
    console.log(compute_witness_command);
    const output = execSync(compute_witness_command, {cwd: '/home/akash/Videos/MIT-Blockchain-Hackathon-2020/zk_library/password2', encoding: 'utf-8', maxBuffer: 10000*1024});
    let command1 = "grep '~out_0' witness";
    let command2 = "grep '~out_1' witness";
    const output1 = execSync(command1, {cwd: '/home/akash/Videos/MIT-Blockchain-Hackathon-2020/zk_library/password2', encoding: 'utf-8' });
    const output2 = execSync(command2, {cwd: '/home/akash/Videos/MIT-Blockchain-Hackathon-2020/zk_library/password2', encoding: 'utf-8' });
    let out0 = BigNumber(output1.split(" ")[1]).toString(16);
    let out1 = BigNumber(output2.split(" ")[1]).toString(16);
    let finalHash = out0 + out1;
    console.log("here", finalHash);
    return finalHash;
    
}



async function main(){

    let age_proof = await generate_age_proof(21, 18);
    console.log(JSON.stringify(password_proof));
    let hash = await get_hash(5);
    console.log(hash);
    let password_proof = generate_password_proof(5, hash);
    console.log(JSON.stringify(password_proof));
}

module.exports = {

    generate_age_proof,
    generate_password_proof,
    get_hash,
    generate_vote_proof

}