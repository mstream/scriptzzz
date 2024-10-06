self.onmessage = function(event) {
  const code = event.data
  console.log(`Executing script: ${code}`)
  const fn = new Function(code)
  try {
    const output = JSON.stringify(fn())
    self.postMessage({ data: output, kind: 'value' });
  } catch (error) {
    const errorMessage = error instanceof Error ?
      error.message :
      'unexpected error'

    self.postMessage({ data: errorMessage, kind: 'exception' });
  }
}

