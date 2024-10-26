mindmap %% mermaid.live markdown file to explain AWS bucket/ff-folder repository structure
  root((ff-folder))
    models
    ::icon(fa fa-folder-plus)
        some-Countries/Region<br/>e.g. South America 1
        ::icon(fa fa-folder)
            CR_name.model
            CR_name.rda
            CR_name_small.model
            CR_name_small.rda
    predictions
    ::icon(fa fa-folder-plus)
        some-iso3<br/>_countrynames<br/>e.g. IDN
        ::icon(fa fa-folder-open)
            iso3-countryname<br/>_somedates.tif
    preprocessed
    ::icon(fa fa-folder-plus)
        groundtruth
        ::icon(fa fa-folder-plus)
            some-coordinate-tiles<br/>e.g. 00N_150E
            ::icon(fa fa-folder-open)
                tile-coordinate<br>_some-dates_groundtruth<br/>1m/3m/6m/12m.tif<br/>e.g. 00N_150E_2021-08-01<br/>_groundtruth6m.tif
        input
        ::icon(fa fa-folder-plus)
            some-coordinate-tiles
            ::icon(fa fa-folder-open)
                tile-coordinate<br>_some-dates<br/>_dataname.tif<br/>e.g. 00N_040E_2020-01-01<br/>_closenesstococoa.tif

    %%     experimentation
    %%     ::icon(fa fa-folder-slash)
    %%     year-month-TestingTimeIntervals
    %%     ::icon(fa fa-folder-plus)
    %%     year-month-TestingAmounts
    %%     machine_learning_alternative
    %%     models_param
    %%         some-Countries/Region<br/>_bestparam.model
    %%         some-Countries/Region<br/>_bestparam.rda
    %%     some_experiment.csv
    %%     ::icon(fa fa-file-csv)
    %% accuracy-analysis
    %% ::icon(fa fa-folder-plus)
    %%     some-date(s)
    %%     ::icon(fa fa-folder-minus)
    %%     amounts
    %%     ::icon(fa fa-folder-open)
    %%         some_binpred.csv
    %%         ::icon(fa fa-file-csv)
    %%     resultaten
    %%     ::icon(fa fa-folder-plus)
    %%         some_training.csv
    %%         ::icon(fa fa-file-csv)
    %%         result
    %%         ::icon(fa fa-folder-open)
    %%             bestmethods.csv
    %%             ::icon(fa fa-file-csv)
    %% contextualization
    %%     ECOBIOME
    %%     ::icon(fa fa-folder-open)
    %%     GADM
    %%     ::icon(fa fa-folder-open)
    %%     WDPA
    %%     ::icon(fa fa-folder-open)