const decoder = new TextDecoder("utf8");
const encoder = new TextEncoder();

const elmFile = await Deno.readFile("./src/elm.js");
const elmJS = decoder.decode(elmFile);

try {
  const globalEval = eval;
  globalEval(elmJS);
} catch (err) {
  console.log("carl", err);
}

await Deno.mkdir("dist/js", { recursive: true });
await Deno.mkdir("dist/zig", { recursive: true });

// const kernelFile = await Deno.readFile("./core/Bex/Kernel.js");
// const kernelCode = decoder.decode(kernelFile);

// @ts-ignore
const app = globalThis.Elm.Main.init({
  flags: Deno.args,
});

app.ports.log.subscribe(function (msg: string) {
  console.log(msg);
});

app.ports.logError.subscribe(function (msg: string) {
  console.error(msg);
  Deno.exit(1);
});

app.ports.loadFile.subscribe(async function (filePath: string) {
  try {
    const file = await Deno.readFile(filePath);
    app.ports.fileLoaded.send({
      path: filePath,
      content: decoder.decode(file),
    });
  } catch (err) {
    console.log("loadFile", filePath, err);
  }
});

interface WriteMsg {
  filePath: string;
  content: string;
}

app.ports.compilerOutput.subscribe(async function ({
  filePath,
  content,
}: WriteMsg) {
  try {
    await Deno.writeFile(filePath, encoder.encode(content));
  } catch (err) {
    console.log("writeFile", filePath, err);
  }
});

// app.ports.loadConfig.subscribe(async function (filePath: string) {
//   try {
//     const config = await Deno.readFile(filePath);
//     app.ports.configLoaded.send(JSON.parse(decoder.decode(config)));
//   } catch (err) {
//     console.log("loadConfig", filePath, err);
//   }
// });

// app.ports.loadKernel.subscribe(async function (filePath: string) {
//   try {
//     const file = await Deno.readFile(filePath);
//     app.ports.kernelLoaded.send(decoder.decode(file));
//   } catch (err) {
//     console.log("loadKernel", filePath, err);
//   }
// });

// interface WriteMsg {
//   filePath: string;
//   content: string;
// }

// app.ports.writeFile.subscribe(async function ({ filePath, content }: WriteMsg) {
//   try {
//     await Deno.writeFile(filePath, encoder.encode(content));
//   } catch (err) {
//     console.log("writeFile", filePath, err);
//   }
// });

// app.ports.status.subscribe(function (status: string) {
//   console.log(status);
// });
