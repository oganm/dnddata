#!/bin/bash


parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
cd "$parent_path"

Rscript dataProcess.R > dataProcess.log 2>dataProcessErr.log
Rscript renderReadme.R > renderReadme.log 2>renderReadmeErr.log
Rscript pushGithub.R > pushGithub.log 2>pushGithubErr.log
