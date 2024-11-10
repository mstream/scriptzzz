function exceptionMessage(errorMessage) {
  return { data: errorMessage, kind: 'exception' }
}

function valueMessage(value) {
  return { data: JSON.stringify(value), kind: 'value' }
}

self.onmessage = function(event) {
  const code = event.data
  console.log(`Executing script: ${code}`)
  const fn = new Function(code)
  try {
    self.postMessage(valueMessage(fn()));
  } catch (error) {
    if (error instanceof Error) {
      self.postMessage(exceptionMessage(error.message));
    } else {
      self.postMessage(exceptionMessage('unknown error'));
    }
  }
}

