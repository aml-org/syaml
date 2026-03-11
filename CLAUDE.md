# syaml

**Repository**: https://github.com/aml-org/syaml

## Release Process
**Does NOT follow the regular release process.**

## Release Position
- Released **outside the regular release line** (when needed)
- Check if new release is needed before starting main release line
- Must be adopted in **amf-core** before amf-core release if there's a new version

## Branch Structure
**No 'develop' branch** - uses feature branch workflow:
1. Work in feature branches
2. Create PR to merge to `master`
3. Master automatically publishes new patch version based on Jenkins run number

## Files & Locations

### Version File
- `build.sbt` - specifies dependencies and major/minor version of the project

### Dependencies
- scala-common

## Versioning
- Major/minor version specified in `build.sbt`
- Patch version is **automatic** based on Jenkins run number
- Each merge to master triggers automatic publish

## Release Steps

### Publishing New Version
1. Create feature branch: `git checkout master && git pull && git checkout -b feature/description`
2. Make changes
3. Update major/minor version in `build.sbt` if needed
4. Update scala-common dependency if needed
5. Commit changes: `git commit -m "Description of changes"`
6. Push: `git push -u origin feature/description`
7. Create PR to merge into `master`
8. When PR is merged, Jenkins automatically publishes new patch version

### No Manual Versioning
- Patch version is controlled by Jenkins run number
- No need to manually set full version
- Just update major/minor in build.sbt when needed

## Dependencies Impact
After releasing syaml, update the version in:
- amf-core (before amf-core release)

## Notes
- Does not need to follow the release times of the rest of the projects
- Released only when needed
- Automatic patch versioning on every master merge
- No RC or manual release process
