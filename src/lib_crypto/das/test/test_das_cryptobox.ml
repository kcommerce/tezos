module Test = struct
  module Scalar = Bls12_381.Fr

  let random_indices bound k =
    Random.self_init () ;

    let rand_elt l =
      let rec loop i = function
        | true -> i
        | false ->
            let n = Random.int bound in
            loop n (not @@ List.mem n l)
      in
      loop 0 false
    in

    let rec aux l n =
      match List.length l with
      | x when x = n -> l
      | _ -> aux (rand_elt l :: l) n
    in

    aux [] k

  (* Encoding and decoding of Reed-Solomon codes on the erasure channel *)
  let bench_DAS_crypto_params () =
    let shards_amount = 2048 in
    let slot_size = 1000000 in
    let msg_size = slot_size in
    let msg = Bytes.create msg_size in
    for i = 0 to (msg_size / 64) - 1 do
      Bytes.set_int64_le msg i (Random.int64 Int64.max_int)
    done ;
    let open Tezos_error_monad.Error_monad.Result_syntax in
    List.iter
      (fun redundancy_factor ->
        Printf.printf "\nk = 2^15 ; n = %d * k\n" redundancy_factor ;

        let module DAS_crypto = Das_cryptobox.Make (struct
          let redundancy_factor = redundancy_factor

          let slot_size = slot_size

          let slot_segment_size = 4096

          let shards_amount = shards_amount
        end) in
        match
          let* p = DAS_crypto.polynomial_from_bytes msg in
          let time = Unix.gettimeofday () in
          let* cm = DAS_crypto.commit p in
          Printf.printf "\ncommit : %f s.\n" (Unix.gettimeofday () -. time) ;

          let time = Unix.gettimeofday () in
          let* pi =
            DAS_crypto.prove_slot_segment
              p
              ~slot:msg
              ~offset:(31 * 2)
              ~length:(31 * 132)
          in
          Printf.printf
            "\nsegment proof : %f s.\n"
            (Unix.gettimeofday () -. time) ;
          let time = Unix.gettimeofday () in
          let slot_segment = Bytes.sub msg (31 * 2) (31 * 132) in
          assert (
            DAS_crypto.verify_slot_segment cm ~slot_segment ~offset:(31 * 2) pi) ;
          Printf.printf
            "\nverify_segment : %f s.\n"
            (Unix.gettimeofday () -. time) ;

          let enc_shards = DAS_crypto.to_shards p in

          Printf.printf "\nencoding : %f s.\n" (Unix.gettimeofday () -. time) ;

          (* Only take half of the buckets *)
          let c_indices = random_indices (2048 - 1) 1024 |> Array.of_list in

          let c =
            DAS_crypto.IntMap.filter
              (fun i _ -> Array.mem i c_indices)
              enc_shards
          in

          let time = Unix.gettimeofday () in
          let* dec = DAS_crypto.from_shards c in

          let _min a b = if a > b then b else a in
          assert (
            Bytes.compare
              msg
              (Bytes.sub
                 (DAS_crypto.polynomial_to_bytes dec)
                 0
                 (min slot_size msg_size))
            = 0) ;

          Printf.printf "\ndecoding : %f s.\n" (Unix.gettimeofday () -. time) ;

          let time = Unix.gettimeofday () in
          let* comm = DAS_crypto.commit p in
          Printf.printf "\ncommit: %f s.\n" (Unix.gettimeofday () -. time) ;
          let time = Unix.gettimeofday () in
          let shard_proofs = DAS_crypto.prove_shards p in
          Printf.printf
            "\nproofs for shards : %f s.\n"
            (Unix.gettimeofday () -. time) ;

          match DAS_crypto.IntMap.find 0 enc_shards with
          | None -> Ok ()
          | Some eval ->
              let time = Unix.gettimeofday () in
              assert (DAS_crypto.verify_shard comm (0, eval) shard_proofs.(0)) ;
              Printf.printf
                "\nverify shard n°0: %f s.\n"
                (Unix.gettimeofday () -. time) ;

              let time = Unix.gettimeofday () in
              let* pi =
                DAS_crypto.prove_degree p (DAS_crypto.polynomial_degree p)
              in
              Printf.printf
                "\nprove degree : %f s.\n"
                (Unix.gettimeofday () -. time) ;
              let time = Unix.gettimeofday () in
              let* check =
                DAS_crypto.verify_degree
                  comm
                  pi
                  (DAS_crypto.polynomial_degree p)
              in
              assert check ;
              Printf.printf
                "\nverify degree: %f s.\n"
                (Unix.gettimeofday () -. time) ;

              let time = Unix.gettimeofday () in
              let point = Scalar.random () in
              let* pi_slot = DAS_crypto.prove_single p point in
              Printf.printf
                "\nprove single: %f s.\n"
                (Unix.gettimeofday () -. time) ;

              let time = Unix.gettimeofday () in
              assert (
                DAS_crypto.verify_single
                  comm
                  ~point
                  ~evaluation:(DAS_crypto.polynomial_evaluate p point)
                  pi_slot) ;
              Printf.printf
                "\nverify single: %f s.\n"
                (Unix.gettimeofday () -. time) ;

              let time = Unix.gettimeofday () in
              let points = [Scalar.random (); Scalar.random ()] in
              let evaluations =
                List.map (DAS_crypto.polynomial_evaluate p) points
              in
              let+ proofs_multi = DAS_crypto.prove_multi p points in
              Printf.printf
                "\nprove multi: %f s.\n"
                (Unix.gettimeofday () -. time) ;
              let time = Unix.gettimeofday () in
              assert (
                DAS_crypto.verify_multi comm ~points ~evaluations proofs_multi) ;
              Printf.printf
                "\nverify multi: %f s.\n"
                (Unix.gettimeofday () -. time)
        with
        | Ok () -> ()
        | Error _ ->
            Format.eprintf "\nError!\n" ;
            assert false)
      [2]
end

let test =
  [Alcotest.test_case "test_das_cryptobox" `Quick Test.bench_DAS_crypto_params]

let () =
  (* Seed for deterministic pseudo-randomness:
      If the environment variable RANDOM_SEED is set, then its value is used as
      as seed. Otherwise, a random seed is used.
     WARNING: using [Random.self_init] elsewhere in the tests breaks thedeterminism.
  *)
  (*Memtrace.trace_if_requested ~context:"Test" () ;*)
  let seed =
    match Sys.getenv_opt "RANDOM_SEED" with
    | None ->
        Random.self_init () ;
        Random.int 1073741823
    | Some v -> int_of_string v
  in
  Printf.printf "Random seed: %d\n" seed ;
  Random.init seed ;
  Alcotest.run "Kate Amortized" [("DAScryptobox", test)]
