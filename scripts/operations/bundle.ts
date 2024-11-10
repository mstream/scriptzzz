import * as esbuild from "esbuild"
import { esbuildCommonBuildOptions } from "../common.ts"

export default async function(): Promise<void> {
  await esbuild.build({
    ...esbuildCommonBuildOptions,
    drop: ['debugger'],
    logLevel: 'info',
    minify: true,
    target: 'es2020',
    treeShaking: true,
  })
}
