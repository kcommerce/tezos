let _ =
  let open Lwt.Syntax in
  let* x1 = (Lwt.return 1 [@time.flush]) in
  let* x2 = (Lwt.return 2 [@time.flush]) in
  let* x3 = (Lwt.return 3 [@time.flush]) in
  let* x4 = (Lwt.return 4 [@time.flush]) in
  let* x5 = (Lwt.return 5 [@time.flush]) in
  Lwt.return (x1 + x2 + x3 + x4 + x5)
