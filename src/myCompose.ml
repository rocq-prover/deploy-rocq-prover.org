open Lwt.Infix

type t = { pull : bool }

let id = "docker-compose"

module Key = struct
  type t = {
    name : string;
    compose_file : string;
    docker_context : string option;
    hash: string;
    env : string array
  } [@@deriving to_yojson]

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = struct
  type t = {
    cwd : string;
  } [@@deriving to_yojson]

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Outcome = Current.Unit

let cmd args { Key.docker_context; name; hash = _; compose_file; env = _ } =
  MyCmd.compose ~docker_context (["-f"; compose_file; "-p"; name ] @ args)

let cmd_pull = cmd ["pull"]

let cmd_update = cmd ["up"; "-d"; "--build"; "--force-recreate"]

let cmd_prune key =
  MyCmd.docker ~docker_context:key.Key.docker_context ["image"; "prune"; "-f"; "--filter"; "until=24h"]

let publish { pull } job key {Value.cwd} =
  Current.Job.start job ~level:Current.Level.Dangerous >>= fun () ->
  let p =
    Current.Job.log job "Working directory: %S" cwd;
    if pull then Current.Process.exec ~cwd:(Fpath.v cwd) ~stdin:"" ~cancellable:true ~job (cmd_pull key)
    else Lwt.return (Ok ())
  in
  p >>= function
  | Error _ as e -> Lwt.return e
  | Ok () -> 
    Current.Job.log job "Working directory: %S" cwd;
    let () =
      let envfile = open_out (cwd ^ "/.env") in
      let s = String.concat "\n" (Array.to_list key.Key.env) in
      output_string envfile s; output_char envfile '\n'; close_out envfile
    in
    Current.Process.exec ~cwd:(Fpath.v cwd) ~stdin:"" ~cancellable:true ~job (cmd_update key) >>= function
    | Error _ as e -> Lwt.return e
    | Ok () -> Current.Process.exec ~cwd:(Fpath.v cwd) ~stdin:"" ~cancellable:true ~job (cmd_prune key)

let pp f (key, { Value.cwd }) =
  Fmt.pf f "%a@.@[cwd: %a@]" MyCmd.pp (cmd_update key) Fmt.string cwd

let auto_cancel = false
