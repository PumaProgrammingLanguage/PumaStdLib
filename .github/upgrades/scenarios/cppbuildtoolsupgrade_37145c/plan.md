# Plan: Fix Build Break After `Types` → `Type` / `PumaTypes` → `PumaType` Rename

## 1. Project Metadata

- **Solution**: `C:\Users\dabur\source\repos\PumaStdLib\PumaStdLib.sln`
- **Branch (current)**: `main`
- **Planning Stage**: C++ Build Tools Upgrade scenario
- **Assessment File**: `C:\Users\dabur\source\repos\PumaStdLib\.github\upgrades\scenarios\cppbuildtoolsupgrade_37145c\assessment.md`
- **Plan File**: `C:\Users\dabur\source\repos\PumaStdLib\.github\upgrades\scenarios\cppbuildtoolsupgrade_37145c\plan.md`

## 2. Executive Summary

The current build break is caused by a partial rename. Source code has mostly moved to `Type`/`PumaType`, but solution/project metadata still contains `PumaTypes` paths/names.

From latest Build output pane:
- `C:\Users\dabur\source\repos\PumaStdLib\PumaTypes\PumaTypes.vcxproj : error MSB4025: Could not find file ...`

Root mismatch confirmed:
- Existing project file: `C:\Users\dabur\source\repos\PumaStdLib\PumaType\PumaTypes.vcxproj`
- Solution still points to missing path: `PumaTypes\PumaTypes.vcxproj`

User-requested target state is full rename consistency:
- namespace `Types` → `Type`
- folder `Types`/`PumaTypes` conventions aligned to `Type`/`PumaType`
- project/output naming `PumaTypes` → `PumaType` (including `.lib`, `.pdb`, `.idb` names)

## 3. In-Scope vs Out-of-Scope Issues

### In-Scope (requested)

1. **Build-stopping missing project file path**
   - Issue: `error MSB4025`
   - Related path in issue: `C:\Users\dabur\source\repos\PumaStdLib\PumaTypes\PumaTypes.vcxproj`
   - Needed fix direction: align solution project entry to actual renamed location/name.

2. **Project identity/output rename consistency**
   - Related file: `C:\Users\dabur\source\repos\PumaStdLib\PumaType\PumaTypes.vcxproj`
   - Observed mismatch: `<RootNamespace>PumaTypes</RootNamespace>` and project filename still `PumaTypes.vcxproj`
   - Needed fix direction: normalize project/file/output naming to `PumaType` so generated artifacts use expected names (`PumaType.lib`, `PumaType.pdb`, `PumaType.idb`).

3. **Cross-project reference/include consistency after rename**
   - Related files:
     - `C:\Users\dabur\source\repos\PumaStdLib\PumaConsole\Console.hpp`
     - `C:\Users\dabur\source\repos\PumaStdLib\PumaFile\Directory.hpp`
     - `C:\Users\dabur\source\repos\PumaStdLib\PumaFile\Text.hpp`
   - Current state: includes already reference `..\PumaType\...`; must validate no lingering old-path references.

### Out-of-Scope (explicitly not requested now)

1. Any warnings not required to complete rename/build recovery.
2. Refactors unrelated to naming/path correction.
3. Build-tools modernization beyond current rename/build-fix scope.

## 4. Investigation Results (Planning Iterations)

### Iteration 1.1 — Build evidence collection
- Attempted structured collection with `cppupgrade_rebuild_and_get_issues` (COM failure in this IDE session).
- Used Build Output pane as source of truth for current blocker.
- Confirmed one failing project while other two succeeded.

### Iteration 1.2 — Solution/project graph verification
- Topological order tool still reports stale project path:
  - `C:\Users\dabur\source\repos\PumaStdLib\PumaTypes\PumaTypes.vcxproj`
- Actual project file exists at:
  - `C:\Users\dabur\source\repos\PumaStdLib\PumaType\PumaTypes.vcxproj`

### Iteration 1.3 — Rename impact verification
- Consumer headers already include from `..\PumaType\...`.
- Core type namespace already appears as `namespace Type` in inspected files.
- Project identity still partly old (`PumaTypes` in project filename/root namespace).

## 5. Strategy Options

### Option A (Minimal unblock)
- Update `.sln` path only to `PumaType\PumaTypes.vcxproj`.
- **Pros**: fastest restore of build graph.
- **Cons**: does not satisfy full `PumaTypes`→`PumaType` rename intent; artifacts likely still old-named.

### Option B (Recommended, matches your request)
- Complete rename normalization in one controlled pass:
  1. Solution project display/path rename to `PumaType` and `PumaType\PumaType.vcxproj`.
  2. Rename project file `PumaTypes.vcxproj` → `PumaType.vcxproj` (and filters/user files accordingly).
  3. Update project metadata (`RootNamespace`, and if needed `TargetName`, `ProgramDataBaseFileName`, `IntDir`/`OutDir` naming) to emit `PumaType` artifacts.
  4. Validate cross-project includes/references and rebuild.
- **Pros**: consistent naming in source/project/artifacts; aligns with your stated goal.
- **Cons**: slightly larger edit surface.

### Option C (Two-step safety rollout)
- Step 1 minimal unblock, Step 2 full rename in next commit.
- **Pros**: easier rollback between steps.
- **Cons**: temporary inconsistent state.

## 6. Selected Strategy

**Selected: Option B** (full consistency), because it directly matches your request and removes recurring naming drift.

## 7. Execution Plan (Dependency-Ordered)

1. **Create new working branch** (do not commit on `main`).
2. **Fix solution mapping first** (`.sln` project display name/path to `PumaType`).
3. **Project file rename and metadata alignment**:
   - Unload project before editing `.vcxproj`.
   - Rename `PumaTypes.vcxproj` and related files to `PumaType.*`.
   - Update `RootNamespace` and output naming settings for `.lib/.pdb/.idb` consistency.
   - Validate `.vcxproj`, then reload project.
4. **Reference verification pass**:
   - Ensure `PumaConsole`/`PumaFile` include/project references remain valid.
5. **Build validation**:
   - Run `cppupgrade_rebuild_and_get_issues` and compare against out-of-scope baseline to confirm no new issues introduced.

## 8. Risks and Mitigations

- **Risk**: project reload failure after `.vcxproj` edits.  
  **Mitigation**: mandatory unload → edit → `cppupgrade_validate_vcxproj_file` → reload sequence.

- **Risk**: artifact names not changing despite project rename.  
  **Mitigation**: explicitly set/verify output-related properties in `PumaType.vcxproj` and re-check build output paths.

- **Risk**: stale references in solution metadata.  
  **Mitigation**: verify solution entry and GUID mapping after rename.

## 9. Acceptance Criteria

- Solution builds without MSB4025 missing-project errors.
- `PumaType` project loads and builds from renamed path/file.
- Output artifacts are named per request (`PumaType.lib`, `PumaType.pdb`, `PumaType.idb`) for tested configuration(s).
- No new in-scope errors introduced.

## 10. Planning Checklist

- [x] Build failure evidence captured.
- [x] Root cause identified with exact paths.
- [x] In-scope and out-of-scope issues listed.
- [x] Options and trade-offs provided.
- [x] Recommended strategy selected.
- [x] Dependency-ordered execution sequence defined.
- [x] Validation and risk mitigations defined.

---

Ready to generate `tasks.md` and start execution automatically using this plan.
