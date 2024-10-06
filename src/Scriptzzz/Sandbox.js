export function mockErrorEventImpl() {
  return new Event('ErrorEventMock')
}

export function mockMessageEventImpl(data) {
  return new MessageEvent('MessageEventMock', { data })
}
