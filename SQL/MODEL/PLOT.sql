SELECT PLOT.CN PLT_CN,
    SUM((TREE.FREMVCFAL * TREE.TPAREMV_UNADJ * CASE
             WHEN GRM.SUBPTYP_GRM = 1 THEN
              POP_STRATUM.ADJ_FACTOR_SUBP
             WHEN GRM.SUBPTYP_GRM = 2 THEN
              POP_STRATUM.ADJ_FACTOR_MICR
             WHEN GRM.SUBPTYP_GRM = 3 THEN
              POP_STRATUM.ADJ_FACTOR_MACR
             WHEN GRM.SUBPTYP_GRM IS NULL THEN
              CASE
                WHEN TREE.DIA IS NULL THEN
                 POP_STRATUM.ADJ_FACTOR_SUBP
                ELSE
                 CASE LEAST(TREE.DIA, 5 - 0.001)
                   WHEN TREE.DIA THEN
                    POP_STRATUM.ADJ_FACTOR_MICR
                   ELSE
                    CASE LEAST(TREE.DIA,
                           COALESCE(PLOT.MACRO_BREAKPOINT_DIA, 9999) - 0.001)
                      WHEN TREE.DIA THEN
                       POP_STRATUM.ADJ_FACTOR_SUBP
                      ELSE
                       POP_STRATUM.ADJ_FACTOR_MACR
                    END
                 END
              END
           END)) AS REMOVALS,
       COND.TRTCD1,
       PLOT.ECOSUBCD,
       PLOT.STATECD,
   PLOT.RDDISTCD,
PLOT.REMPER,
        PCOND.BALIVE,
       PCOND.STDORGCD,
PCOND.SITECLCD,
    PCOND.STDSZCD,
       PCOND.FORTYPCD       
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
  JOIN FS_FIADB.SDS_COND_VW COND
    ON (COND.PLT_CN = PLOT.CN)
  JOIN FS_FIADB.TREE TREE
    ON (TREE.PLT_CN = PLOT.CN AND TREE.CONDID = COND.CONDID)
  JOIN FS_FIADB.TREE_GRM_ESTN GRM
    ON (TREE.CN = GRM.TRE_CN)
  JOIN FS_FIADB.PLOT PPLOT
    ON (PPLOT.CN = PLOT.PREV_PLT_CN)
  JOIN FS_FIADB.SDS_COND_VW PCOND
    ON (PLOT.PREV_PLT_CN = PCOND.PLT_CN AND TREE.PREVCOND = PCOND.CONDID)
  LEFT OUTER JOIN FS_FIADB.TREE PTREE
    ON (TREE.PREV_TRE_CN = PTREE.CN)
 WHERE GRM.ESTIMATE = 'VOLUME'
   AND GRM.ESTN_TYPE = 'AL'
   AND GRM.ESTN_UNITS = 'CF'
   AND GRM.LAND_BASIS = 'FORESTLAND'
   AND TREE.TPAREMV_UNADJ IS NOT NULL
   AND TREE.FREMVCFAL IS NOT NULL
   AND PET.EVAL_TYP = 'EXPREMV'
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
   AND COND.COND_STATUS_CD = 1
   AND COND.CONDID = 1
   AND COND.OWNCD = 45
   AND 1 = 1
 GROUP BY PLOT.CN,
          COND.TRTCD1,
       PLOT.ECOSUBCD,
       PLOT.STATECD,
   PLOT.RDDISTCD,
PLOT.REMPER,
        PCOND.BALIVE,
       PCOND.STDORGCD,
PCOND.SITECLCD,
    PCOND.STDSZCD,
       PCOND.FORTYPCD 
