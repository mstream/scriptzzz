import { detailedDiff } from 'deep-object-diff';
import verror from 'verror'

const { VError } = verror

function areEqual(diff) {
  const addedCount = Object.keys(diff.added).length
  const deletedCount = Object.keys(diff.deleted).length
  const updatedCount = Object.keys(diff.updated).length
  return addedCount === 0 && deletedCount === 0 && updatedCount === 0
}

export function checkEqualImpl(description) {
  return (actual) => {
    return (expected) => {
      return () => {
        if (typeof actual !== "object" || typeof expected !== "object") {
          if (actual !== expected) {
            throw new VError(
              {
                name: 'EqualAssertionFailed',
                info: {
                  actual,
                  expected,
                },
              },
              `${description} actual value differs from expected`
            )
          }
        }
        const diff = detailedDiff(expected, actual)
        if (!areEqual(diff)) {
          throw new VError(
            {
              name: 'EqualAssertionFailed',
              info: {
                actual,
                expected,
                diff,
              },
            },
            `${description} actual value differs from expected`
          )
        }
      }
    }
  }
}

export function showErrorImpl(error) {
  if (error instanceof VError) {
    return [
      error.name,
      error.message,
      JSON.stringify(VError.info(error), null, 2),
    ].join('\n')
  } else if (error instanceof Error) {
    return error.message
  } else {
    return "<UNKNOWN>"
  }
}
