type runtime_event_policy = Conservative | Report | Ignore
type disposition = Bug | Diagnostic | Ignore_event

let runtime_event_disposition policy severity =
  match (policy, severity) with
  | Ignore, _ -> Ignore_event
  | Report, _ -> Diagnostic
  | Conservative, (Error.Runtime_event.Warning | Error) -> Bug
  | Conservative, (Notice | Deprecation) -> Diagnostic
