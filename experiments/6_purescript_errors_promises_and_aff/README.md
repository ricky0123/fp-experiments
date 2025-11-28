1. Use basic PureScript + Aff to do something that succeeds
1. Use toAffE + FFI, imported into Aff, to do something that succeeds
1. Use toAffE + FFI, imported into Aff, to do something that fails
1. Use toAffE + FFI + "attempt" to catch failure
1. Use fromEffectFnAff to do something that succeeds
1. Use fromEffectFnAff to do something that fails
1. Use fromEffectFnAff + attempt to catch failure

Notes:

- `show someError` always shows the stack trace
