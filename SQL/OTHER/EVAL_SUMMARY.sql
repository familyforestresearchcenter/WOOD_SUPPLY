SELECT PLOT.STATECD, PLOT.INVYR, PPLOT.INVYR
  FROM FS_FIADB.POP_EVAL_GRP PEG
  JOIN FS_FIADB.POP_EVAL_TYP PET
    ON (PET.EVAL_GRP_CN = PEG.CN)
  JOIN FS_FIADB.POP_EVAL PEV
    ON (PEV.CN = PET.EVAL_CN)
  JOIN FS_FIADB.POP_ESTN_UNIT PEU
    ON (PEV.CN = PEU.EVAL_CN)
  JOIN FS_FIADB.POP_STRATUM POP_STRATUM
    ON (PEU.CN = POP_STRATUM.ESTN_UNIT_CN)
  JOIN FS_FIADB.POP_PLOT_STRATUM_ASSGN POP_PLOT_STRATUM_ASSGN
    ON (POP_PLOT_STRATUM_ASSGN.STRATUM_CN = POP_STRATUM.CN)
  JOIN FS_FIADB.PLOT PLOT
    ON (POP_PLOT_STRATUM_ASSGN.PLT_CN = PLOT.CN)
  JOIN FS_FIADB.PLOTGEOM PLOTGEOM
    ON (PLOT.CN = PLOTGEOM.CN)
  JOIN FS_FIADB.PLOT PPLOT
    ON (PPLOT.CN = PLOT.PREV_PLT_CN)
 WHERE/* GRM.ESTIMATE = 'VOLUME'
   AND GRM.ESTN_TYPE = 'AL'
   AND GRM.ESTN_UNITS = 'CF'
   AND GRM.LAND_BASIS = 'FORESTLAND'*/
  /* AND */PET.EVAL_TYP = 'EXPREMV'
   AND PEG.EVAL_GRP IN (12018,
                        52018,
                        62018,
                        92018,
                        102018,
                        122018,
                        132018,
                        172018,
                        182018,
                        192018,
                        202018,
                        212018,
                        222018,
                        232018,
                        242018,
                        252018,
                        262018,
                        272018,
                        282018,
                        292018,
                        312018,
                        332018,
                        342018,
                        362018,
                        372018,
                        382018,
                        392018,
                        402018,
                        412018,
                        422018,
                        442018,
                        452018,
                        462018,
                        472018,
                        482018,
                        502018,
                        512018,
                        532018,
                        542018,
                        552018)
   AND 1 = 1
 GROUP BY PLOT.STATECD, PLOT.INVYR,  PPLOT.INVYR
