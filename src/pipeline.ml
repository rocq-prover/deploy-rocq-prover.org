module Git = Current_git
module Github = Current_github

let v ~repo () =
  (* Replace this with your actual pipeline: *)
  let src = Git.Local.head_commit repo in
  let src = Git.fetch (Current.map Git.Commit.id src) in
  let src_content = Repo_content.extract src in
  
  Current.ignore_value src
