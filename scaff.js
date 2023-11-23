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

// Make directories if they're not already in place
await Deno.mkdir(`src/${year}`, { recursive: true });
await Deno.mkdir(`input/${year}/real`, { recursive: true });

for (const day of days.split(",").map(n => +n)) {
    // Get problem name
    const page = await fetch(`https://adventofcode.com/${year}/day/${day}`).then(r => r.text());
    const name = /<h2>--- Day \d+: (.+) ---<\/h2>/.exec(page)[1];

    // Make solution file
    const fileText = (await Deno.readTextFile("DayTemplate.fs")).replace(/![A-Z]+?!/g, k => {
        return {
            "!YEAR!": year,
            "!DAY!": day,
            "!NAME!": name
        }[k];
    });
    await Deno.writeTextFile(`src/${year}/Day${day}.fs`, fileText);

    // Add file to project
    const projText = (await Deno.readTextFile("src/aoc.fsproj")).split("\n");
    projText.splice(projText.findIndex(l => l.includes("<!--NEW-->")), 0, `    <Compile Include="${year}/Day${day}.fs" />`);
    await Deno.writeTextFile("src/aoc.fsproj", projText.join("\n"));
    await wait(1500);

    // Get problem input
    const dest = `input/${year}/real/day${day}.txt`;
    if (await exists(dest))
        continue;
    console.log(`Pulling input to ${dest}`)

    const text = await fetch(`https://adventofcode.com/${year}/day/${day}/input`, {
        headers: {
            Cookie: `session=${cookie}`
        }
    }).then(r => r.text());
    await Deno.writeTextFile(dest, text);

    await wait(1500);
}