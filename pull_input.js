#!/usr/bin/env -S deno run --unstable -A

async function exists(path) {
    try {
        await Deno.stat(path);
        return true;
    }
    catch(_) {
        return false;
    }
}

function wait(millis) {
    return new Promise(res => setTimeout(() => res(), millis));
}

const [year, days] = Deno.args;
const cookie = await Deno.readTextFile(".cookie.dat");

await Deno.mkdir(`input/${year}`, { recursive: true });

for (const day of days.split(",").map(n => +n)) {
    const dest = `input/${year}/real/day${day}.txt`;
    if (await exists(dest))
        continue;
    console.log(`Pulling ${dest}`)

    const text = await fetch(`https://adventofcode.com/${year}/day/${day}/input`, {
        headers: {
            Cookie: `session=${cookie}`
        }
    }).then(r => r.text());
    await Deno.writeTextFile(dest, text);
    await wait(1500);
}