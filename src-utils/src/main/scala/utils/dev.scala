package utils

val DEV = true

inline def contract(a: => Boolean) =
  if DEV then
    if !a then
       logicError()

/**
 * something should not happen by contract
 */
def logicError(): Nothing = logicError("")

/**
 * something should not happen by contract
 */
def logicError(additionalInfo: String) = throw new IllegalArgumentException(s"This state is considered a logic error (${additionalInfo})")


/**
 * not something we want to handle by design, mostly code we don't want to design quite well now
 */
def notImplemented() = throw new IllegalArgumentException("Not implemented")
