# ind-illusions

This repository contains all the necessary data, scripts, and materials for the paper:

**Mehrvarz, M., Popat, H., & Rouder, J. N. (2024).**  
*Individual Differences in Visual Illusions: Graphical and Analytic Approaches For Finding Structure in Real-World Cases.*

## Repository Structure
- `/_data`: Raw and formatted data  
- `/_data-processing`: Scripts for wrangling and formatting data  
- `/_figs`: Final figure outputs  
- `/_make-figs`: Scripts to generate figures  
- `/_make-tab`: Scripts to generate manuscript tables  
- `/_manuscript-latex`: LaTeX source for the manuscript  
- `/_media`: Supplemental media files (e.g., images, assets)  
- `/_results`: Output from model estimation (large files excluded)  
- `/_script`: Core analysis and model-running scripts  
- `manuscript.pdf`: Compiled version of the paper  
- `README`: This file

## How to Reproduce

To regenerate key outputs (figures/tables), use the scripts in `_make-figs` and `_make-tab`; Or regenerate model output `_script`
For example:

```r
source("_make-figs/effect-plot.R")
source("_script/bhfm-mod.R")
```

## Citation
To cite this work:
```bibtex
@article{MehrvarzPopatRouder2024,
  title={Individual Differences in Visual Illusions: Graphical and Analytic Approaches For Finding Structure in Real-World Cases},
  author={Mehrvarz, M. and Popat, H. and Rouder, J. N.},
  year={2024}
}
```
## Preprint

The preprint of the paper can be found [here](https://osf.io/preprints/psyarxiv/6z92y_v1).


