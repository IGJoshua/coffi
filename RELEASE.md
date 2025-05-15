# Release Checklist

- [ ] Compile and release jars to maven
- [ ] Tag release on develop as `vX.Y.C` following the versioning scheme for the project
  - The commit number can be found in the name of the uploaded jar
- [ ] Make `release/vX.Y.C` branch, following the versioning scheme for the project, and check it out
- [ ] Update the changelog "Unreleased" section to reflect the new release version
  - [ ] Update the link footnotes with the new diff range
- [ ] Update the readme for the new release version
- [ ] Run the codox alias and commit the changes to documentation
- [ ] Merge the release branch into master
- [ ] Tag the merge commit as `cljdoc-vX.Y.C`
- [ ] Merge the master branch into develop
- [ ] Update the changelog to add a new empty Unreleased section
