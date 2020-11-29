package mlang.utils



def logicError(): Nothing = logicError("no additional info provided")

def logicError(additionalInfo: String) = throw new IllegalArgumentException(s"This state is considered a logic error (${additionalInfo})")
