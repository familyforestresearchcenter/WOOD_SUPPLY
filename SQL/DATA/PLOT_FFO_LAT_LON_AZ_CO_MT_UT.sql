SELECT PLOT.PLT_CN, ACTUAL_LAT, ACTUAL_LON
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
    ON (POP_STRATUM.CN = POP_PLOT_STRATUM_ASSGN.STRATUM_CN)
  JOIN FS_FIADB.SDS_PLOT PLOT
    ON (POP_PLOT_STRATUM_ASSGN.PLT_CN = PLOT.PLT_CN)
  JOIN FS_FIADB.SDS_COND_VW COND
    ON (PLOT.PLT_CN = COND.PLT_CN)
 WHERE 1 = 1
   AND PET.EVAL_TYP = 'EXPCURR'
   AND PEG.EVAL_GRP IN (42018, 82018, 302018, 492018)
   AND 1 = 1
   AND COND.COND_STATUS_CD = 1
   AND COND.OWNCD = 45
 GROUP BY PLOT.PLT_CN, ACTUAL_LAT, ACTUAL_LON
