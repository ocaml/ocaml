This directory contains the configuration files of the Jenkins jobs
used to test OCaml on Inria's continuous integration infrastructure.

Each subdirectory under `tools/ci/inria` corresponds to one CI job
and should contain at least a `Jenkinsfile` describing the pipeline
associated with this job(1). In addition, the job's directory can also
contain a `script` file specifying the commands used to actually execute
the job. Other files may be included as appropriate.

(1) The Jenkinsfiles can follow either the declarative syntax documented
at https://www.jenkins.io/doc/book/pipeline/syntax, or the advanced
(scripted) one documented at
https://www.jenkins.io/doc/book/pipeline/jenkinsfile/#advanced-scripted-pipeline
