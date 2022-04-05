## visualsearch

Code for "A Time Slice Analysis of Dentistry Students’ Visual Search Strategies and Pupil Dilation during Diagnosing Radiographs"

## Data

Data can be obtained [here](https://psycharchives.org/en/item/d080314b-b491-4ccd-af07-1a04f2ccf6fb) and cited via:

```
Borchers, C. (2022). Dataset for: A Time Slice Analysis of Dentistry Students’ Visual Search Strategies and Pupil Dilation during Diagnosing Radiographs [Data set]. PsychArchives. https://doi.org/10.23668/psycharchives.5681
```

BibTex:

```
@misc{Borchers_2022, title={Dataset for: A Time Slice Analysis of Dentistry Students’ Visual Search Strategies and Pupil Dilation during Diagnosing Radiographs}, url={https://psycharchives.org/en/item/d080314b-b491-4ccd-af07-1a04f2ccf6fb}, DOI={10.23668/psycharchives.5681}, publisher={PsychArchives}, author={Borchers, Conrad}, year={2022}, month={Apr.} }
```

## Analysis pipeline

The code is written in a [targets pipeline](https://books.ropensci.org/targets/) and be run via `targets::tar_make()`. 

## Additional Documentation

The folder `documentation/` includes a template of the power analysis we ran for this study as well as code to produce additional analysis reports and plots. Please note that you may have to create additional sub-directories in order to run these scripts successfully.

## Data anonymization 

The data has been anonymized which resulted in all data aggregation procedures for Hypothesis 1 being entirely reproducible while the data for Hypothesis 2 is provided in an anonymized, aggregated format since participant IDs of the diagnostic performance data set were, in part, manually matched. A data anonymization script that may be useful for other studies is provided in the documentation folder. 
