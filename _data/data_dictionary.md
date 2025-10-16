# data dictionary

## columns
- `sub` — subject id (numeric label assigned during merge)
- `task` — task code (see abbreviations below)
- `resp` — raw response value
- `start` — trial start timestamp
- `version` — task version (1 or 2)
- `parA` — task parameter A (reference stimulus)
- `rt` — reaction time in ms (`"null"` set to `8000`, then numeric)
- `time_elapsed` — experiment clock in ms
- `sid` — session id from source logs
- `block` — block order (1 or 2)
- `jitterX` — horizontal jitter in px
- `jitterY` — vertical jitter in px
- `trial` — trial index within task (1–15)
- `y` — signed/normalized score per task (from `makeDat()`)
- `S` — integer subject index (`as.numeric(factor(sub))`)
- `J` — integer task index (`as.numeric(factor(task))`)
- `JV` — task×version index (`(J-1)*2 + version`)

## task abbreviations
- `br` = Brentano  
- `eb` = Ebbinghaus  
- `pog` = Poggendorf  
- `pz` = Panzo  
- `zol` = Zoellner

## csv files
- `../_data/raw-data.csv` — merged from GitHub, filtered to tasks, subjects relabeled, repeated sessions pruned
- `../_data/processed-data.csv` — output of `makeDat()`: computes `y`, fixes `rt`, builds `trial` and `block`
- `../_data/clean_data.csv` — analysis-ready after quality control: removes fast RTs and flagged outliers; leverage diagnostics inspected
