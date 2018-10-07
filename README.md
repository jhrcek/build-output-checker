# Build output checked

A tool for extraction of actionable information from the build output of kiegroup CI jobs.

Input for analysis are plain [console text](https://rhba-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/KIE/job/master/job/dailyBuild/job/kieAllBuild-master/lastSuccessfulBuild/consoleText)
and [console text with timestamps](https://rhba-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/KIE/job/master/job/dailyBuild/job/kieAllBuild-master/lastSuccessfulBuild/timestamps/?appendLog).

## What info are we interested in?

- [ ] Total number of lines - sudden increase in this might indicate tons of new warnings
- [ ] Total duration of the build
- [ ] How long / what proportion of total build time do we spend maven-downloading stuff?
- [ ] Where are we downloading all the stuff from?
- [ ] Are we doing some redundant downloads? Multiple versions of the same thing?
- [ ] Which downloads are slow?
- [ ] What are the largest things we are downloading?
- [ ] Errors and warnings during the build (Maven)
- [ ] Which maven plugins / executions are slow?
- [ ] How long do individual test classes take?

TODO: Why is there less downloads finished than started?
1. 7545 x "[INFO] Downloading from" = Maven starting download of something
2. 6443 x "[INFO] Downloaded from" = Maven finished download of something
