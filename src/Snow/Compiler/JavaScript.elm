module Snow.Compiler.JavaScript exposing (compile)

import Snow.Language exposing (BinOp(..), Literal(..), SnowExpr(..))


compile : SnowExpr -> String
compile expr =
    let
        initializeSteps =
            expr
                |> toString []
                |> List.reverse
                |> String.join ","
    in
    """
const requestAnimationFrame = window.requestAnimationFrame || setTimeout;
function snow() {
  function _Basics_add(left, right) {
      return left + right;
  }

  function _Basics_subtract(left, right) {
      return left - right;
  }

  function _Basics_multiply(left, right) {
      return left * right;
  }

  const gMachine = {
    instructions: [],
    stack: [],
    dump: [],
    heap: new Map(),
    globals: new Map(),
  };
  let pointer = Number.MIN_SAFE_INTEGER;

  function step(instruction) {
    switch(instruction.command) {
        case 'push-int':
            gMachine.heap.set(pointer, { type: 'int', value: instruction.value });
            gMachine.stack.unshift(pointer);
            pointer++;
            break;

        case 'push-global':
            gMachine.globals.set(pointer, { fn: instruction.fn, arity: instruction.arity });
            gMachine.heap.set(pointer + 1, { type: 'global', ptr: pointer })
            gMachine.stack.unshift(pointer + 1);
            pointer += 2;
            break;

        case 'push': {
            const ptr = gMachine.stack[instruction.depth];

            if (ptr !== undefined) {
                gMachine.stack.unshift(ptr)
            } 
        }
        break;

        case 'make-apply': {
            const a0 = gMachine.stack.shift();
            const a1 = gMachine.stack.shift();
            gMachine.heap.set(pointer, { type: 'apply', left: a0, right: a1 });
            gMachine.stack.unshift(pointer);
            pointer++;
        }
        break;
    }
  }

  return {
    run: function() {
        [""" ++ initializeSteps ++ """].forEach(step);

        unwind: while(true) {
            try {
                const next = gMachine.stack[0];
                const val = gMachine.heap.get(next);

                switch(val.type) {
                    case 'apply':
                        gMachine.stack.unshift(val.left);
                        break;

                    case 'global': {
                        const global = gMachine.globals.get(val.ptr);

                        if (gMachine.stack.length <= global.arity) {
                            throw new Error(`Expected ${global.arity} items on the stack but found ${gMachine.stack.length}`);
                        }

                        let data = [];

                        for (let i = 1; i <= global.arity; i++) {
                            const heapPtr = gMachine.stack[i];
                            const argApp = gMachine.heap.get(heapPtr);
                            const arg = gMachine.heap.get(argApp.right);

                            data.unshift(arg.value);
                        }

                        gMachine.heap.set(pointer, { type: 'int', value:  global.fn(...data) });
                        gMachine.stack.unshift(pointer);
                        // Slide
                        gMachine.stack = gMachine.stack.splice(global.arity + 1);
                        gMachine.stack.unshift(pointer);

                        pointer++;
                    }
                    break;

                    default:
                        break unwind;
                }
            } catch(err) {
                console.log('Error', gMachine);
                return;
            }
        }

        return gMachine.heap.get(gMachine.stack[0]).value;
    }
  };
}

// DEBUG

const app = snow();
console.log('result', app.run());"""


toString : List String -> SnowExpr -> List String
toString instructions expr =
    case expr of
        SVar id ->
            instructions

        SLambda id body ->
            instructions

        SApply left right ->
            toString (toString ("{ command: 'make-apply' }" :: instructions) left) right

        SLiteral (LInt i) ->
            ("{ command: 'push-int', value: " ++ String.fromInt i ++ " }") :: instructions

        SBinOp op left right ->
            compileBinOp op ++ toString (toString instructions left) right


compileBinOp : BinOp -> List String
compileBinOp op =
    case op of
        Addition ->
            [ "{ command: 'make-apply' }"
            , "{ command: 'make-apply' }"
            , "{ command: 'push-global', fn: _Basics_add, arity: 2 }"
            ]

        Subtraction ->
            [ "{ command: 'make-apply' }"
            , "{ command: 'make-apply' }"
            , "{ command: 'push-global', fn: _Basics_subtract, arity: 2 }"
            ]

        Multiplication ->
            [ "{ command: 'make-apply' }"
            , "{ command: 'make-apply' }"
            , "{ command: 'push-global', fn: _Basics_multiply, arity: 2 }"
            ]
