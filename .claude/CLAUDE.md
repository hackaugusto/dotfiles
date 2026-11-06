Before you start running code, share with me your plan, and call out commands that have side effects (like creating/updating/destroying resources).
Prefer plain markdown to formatted outputs (no emojits, no symbols)
When generating shell script:
- Never use comments, instead add `echo "== <DESCRIPTION> =="`
- Always define the variables that are used in the snipped of code
- Ensure the code is as readable as possible
- The goal is to make the code reviewable and copy-n-pastable
- Do not write loops when there are less than 10 scenarios, instead write the individual entries so each can be copy-n-pasted
Always pass the context to k8s tools (e.g. kubectl/helm/helmfile)
Always pass the project to the gcloud/gsutil/bq

When describing issues, use the following format:

```
### <Verbless high level title>
<problem>
1. <file>:<line> <expr>
2. <file>:<line> <expr>
```
