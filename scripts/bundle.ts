#! /usr/bin/env -S nix develop --command deno run --allow-env --allow-net --allow-read --allow-run --allow-write

import { runScript } from "./common.ts"
import build from "./operations/build.ts"
import bundle from "./operations/bundle.ts"

runScript(async () => {
  await build()
  await bundle()
}) 
