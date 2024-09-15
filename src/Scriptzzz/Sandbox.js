export function execJsImpl(timeout, code) {
  return () => new Promise((resolve, reject) => {
    setTimeout(() => reject(Error(`Sandbox execution timeout: ${timeout}ms`)), timeout)
    const fn = new Function(code)
    resolve(JSON.stringify(fn()))
  })
}
