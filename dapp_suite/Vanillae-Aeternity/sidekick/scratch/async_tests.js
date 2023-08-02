async function step1()
{
    console.log('step 1');
    throw new Error('step 1');
}

async function step2()
{
    console.log('step 1');
    throw new Error('step 1');
}

async function step3()
{
    console.log('step 1');
    throw new Error('step 1');
}

async function main()
{
    await step1();
    await step2();
    await step3();
}

main();
