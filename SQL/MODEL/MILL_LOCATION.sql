select MILL_STATECD, MILL_TYPE_CD, TOT_MCF, MILL_LAT, MILL_LON
  from fs_nims_tpo.tpo_annual_mill_sample_list
 where mill_status_cd in (1, 2)
   and SURVEY_YEAR = 2018
 order by MILL_STATECD, MILL_TYPE_CD, TOT_MCF
