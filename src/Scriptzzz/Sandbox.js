export function execJsImpl(code) {
  return () => new Promise((resolve) => {
    const fn = new Function(code)
    resolve(fn())
  })
}
