# ocvx

Convex optimization for OCaml. Native bindings to [OSQP](https://osqp.org) and [SCS](https://www.cvxgrp.org/scs/) solvers via ctypes.

## Status

- **ocvx-osqp** — OSQP bindings ✓
- **ocvx-scs** — SCS bindings ✓
- **ocvx** — DSL (in progress)

## Install

Requires OSQP and SCS installed on your system.

```sh
opam pin add ocvx git+https://github.com/youruser/ocvx.git
```

## Usage

### OSQP — Quadratic Programming

Solve `minimize ½x'Px + q'x` subject to `l ≤ Ax ≤ u`:

```ocaml
open Osqp

let () =
  let p = [| [| 4.0; 1.0 |]; [| 1.0; 2.0 |] |] in
  let q = [| 1.0; 1.0 |] in
  let a = [| [| 1.0; 1.0 |]; [| 1.0; 0.0 |]; [| 0.0; 1.0 |] |] in
  let l = [| 1.0; 0.0; 0.0 |] in
  let u = [| 1.0; 0.7; 0.7 |] in
  match setup ~p ~q ~a ~l ~u () with
  | Error e -> print_endline (show_error e)
  | Ok t ->
    match solve t with
    | Error e -> print_endline (show_error e)
    | Ok sol ->
      Array.iteri (fun i v -> Printf.printf "x[%d] = %f\n" i v) sol.x

(* x[0] = 0.301376 *)
(* x[1] = 0.698396 *)
```

### SCS — Conic Programming

Solve `minimize ½x'Px + c'x` subject to `Ax + s = b, s ∈ cone`:

```ocaml
open Scs

let () =
  let p = [| [| 3.0; -1.0 |]; [| -1.0; 2.0 |] |] in
  let c = [| -1.0; -1.0 |] in
  let a = [| [| -1.0; 1.0 |]; [| 1.0; 0.0 |]; [| 0.0; 1.0 |] |] in
  let b = [| -1.0; 0.3; -0.5 |] in
  let cone = { empty_cone with z = 1; l = 2 } in
  match setup ~c ~a ~b ~p ~cone () with
  | Error e -> print_endline (show_error e)
  | Ok t ->
    match solve t with
    | Error e -> print_endline (show_error e)
    | Ok (sol, info) ->
      Array.iteri (fun i v -> Printf.printf "x[%d] = %f\n" i v) sol.x;
      Printf.printf "solved in %d iterations\n" info.iter
```

### Settings

Both solvers expose all upstream settings with sensible defaults:

```ocaml
(* OSQP *)
let settings = { Osqp.default_settings with verbose = false; max_iter = 10000 }
let t = Osqp.setup ~settings ~p ~q ~a ~l ~u ()

(* SCS *)
let settings = { Scs.default_settings with verbose = false; eps_abs = 1e-9 }
let t = Scs.setup ~settings ~c ~a ~b ~p ~cone ()
```

### Warm Starting & Updates

Both solvers support updating problem data and warm starting without reconstructing the workspace:

```ocaml
(* OSQP: update vectors and matrices *)
Osqp.update_vectors t ~q:new_q ~l:new_l ~u:new_u ()
Osqp.update_matrices t ~p:new_p ~a:new_a ()

(* SCS: update b and c vectors, warm start from previous solution *)
Scs.update t ~b:new_b ()
Scs.solve ~warm_start:true ~prev_sol:prev t
```

## Architecture

```
lib/
├── common/     shared utilities (CSC sparse matrices, validation)
├── osqp/       OSQP ctypes bindings + safe API
├── scs/        SCS ctypes bindings + safe API
└── ocvx/       DSL for disciplined convex programming (planned)
```

P matrices are validated for symmetry and automatically converted to upper triangular CSC format. Memory management is handled via `Gc.finalise` — solver workspaces are cleaned up when they become unreachable.

## Building

```sh
opam install ctypes ctypes-foreign ppx_deriving alcotest
dune build
dune runtest
```

## License

MIT
