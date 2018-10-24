# Build output checker

A tool for extraction of actionable information from the build output of kiegroup CI jobs.

Input for analysis are plain [console text with timestamps](https://rhba-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/KIE/job/master/job/dailyBuild/job/kieAllBuild-master/lastSuccessfulBuild/timestamps/?appendLog) and [junitreport](https://rhba-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/KIE/job/master/job/dailyBuild/job/kieAllBuild-master/lastSuccessfulBuild/testReport/api/json?tree=suites%5Bcases%5BclassName%2Cduration%2Cname%5D%5D).

## What info are we interested in?

- [x] Total duration of the build
- [x] How long / what proportion of total build time do we spend maven-downloading stuff?
- [x] How long do individual test classes / methods take?
- [x] Which maven plugins / executions are slow?
- [x] Are there different versions of the same plugin used throughout the build?
- [x] How long does each individual git repo take to build?
- [ ] Total number of lines - sudden increase in this might indicate tons of new warnings
- [ ] Where are we downloading all the stuff from?
- [ ] Are we doing some redundant downloads? Multiple versions of the same thing?
- [ ] Which downloads are slow?
- [ ] What are the largest things we are downloading?
- [ ] Errors and warnings during the build (Maven)
