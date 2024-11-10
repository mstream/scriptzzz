import * as esbuild from "esbuild"
import { esbuildCommonBuildOptions } from "../common.ts"

type Result = {
  host: string,
  port: number,
  stop: () => Promise<void>
}

export default async function(): Promise<Result> {
  const context = await esbuild.context(esbuildCommonBuildOptions)
  const { host, port } = await context.serve({ servedir: 'dist' })
  return { host, port, stop: context.dispose }
}
