# declare a name for this job to be test_job
#PBS -N generate

#PBS -S /bin/bash

# request a total of 1 node and 2 processors for this job
#PBS -l nodes=1:ppn=2

# request 4 hours of wall clock time
#PBS -l walltime=04:00:00

# send emails before the job started and after the job is completed
#PBS -m bea
#PBS -M jxu74@illinois.edu

# set the destination for program's output
#PBS -e localhost:/projects/hackonomics/voting_behavior/outputs/e_outputs/out.txt

# tell PBS the expected memory usage
#PBS -l mem=100gb

# tell PBS the expected run-time
#PBS -l walltime=03:30:00


cd /projects/hackonomics/voting_behavior/outputs/p_outputs


module load R
module load R gdal-stack

Rscript --vanilla /projects/hackonomics/voting_behavior/scripts/workflow_module/generate.R $1 > "${1}_out.txt"
