open Lwt.Infix

type t = { pull : bool }

let id = "docker-compose"

module Key = struct
  type t = {
    name : string;
    compose_file : string;
    docker_context : string option;
  } [@@deriving to_yojson]

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = struct
  type t = {
    cwd : string;
    contents: string;
  } [@@deriving to_yojson]

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Outcome = Current.Unit

let cmd args { Key.docker_context; name; compose_file } =
  MyCmd.compose ~docker_context (["-f"; compose_file; "-p"; name ] @ args)

let cmd_pull = cmd ["pull"]

let cmd_update = cmd ["up"; "-d"]

let publish { pull } job key {Value.cwd; Value.contents} =
  Current.Job.start job ~level:Current.Level.Dangerous >>= fun () ->
  let p =
    Current.Job.log job "Working directory: %S" cwd;
    if pull then Current.Process.exec ~cwd:(Fpath.v cwd) ~stdin:contents ~cancellable:true ~job (cmd_pull key)
    else Lwt.return (Ok ())
  in
  p >>= function
  | Error _ as e -> Lwt.return e
  | Ok () -> 
    Current.Job.log job "Working directory: %S" cwd;
    Current.Process.exec ~cwd:(Fpath.v cwd) ~stdin:contents ~cancellable:true ~job (cmd_update key)

let pp f (key, { Value.cwd; Value.contents }) =
  Fmt.pf f "%a@.@[cwd: %a@]@[%a@]" MyCmd.pp (cmd_update key) Fmt.string cwd Fmt.string contents

let auto_cancel = false
