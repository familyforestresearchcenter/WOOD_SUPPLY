SELECT EVAL_GRP,
       EVAL_GRP_DESCR,
       39 ATTRIBUTE_NBR,
       'AVERAGE ANNUAL REMOVALS OF LIVE TREES (AT LEAST 5 INCHES D.B.H./D.R.C.), IN CUBIC FEET, ON FOREST LAND' ATTRIBUTE_DESCR,
       COALESCE(CAST(GRP_BY_ATTRIB AS VARCHAR(4000)), 'NOT AVAILABLE') GROUP_BY_FIELD,
       SUM(ESTIMATE_BY_ESTN_UNIT.ESTIMATE) ESTIMATE,
       CASE
         WHEN SUM(ESTIMATE_BY_ESTN_UNIT.ESTIMATE) <> 0 THEN
          ABS(SQRT(SUM(ESTIMATE_BY_ESTN_UNIT.VAR_OF_ESTIMATE)) /
              SUM(ESTIMATE_BY_ESTN_UNIT.ESTIMATE) * 100)
         ELSE
          0
       END AS SE_OF_ESTIMATE_PCT,
       SQRT(SUM(ESTIMATE_BY_ESTN_UNIT.VAR_OF_ESTIMATE)) SE_OF_ESTIMATE,
       SUM(ESTIMATE_BY_ESTN_UNIT.VAR_OF_ESTIMATE) VAR_OF_ESTIMATE,
       SUM(ESTIMATE_BY_ESTN_UNIT.TOTAL_PLOTS) TOTAL_PLOTS,
       SUM(ESTIMATE_BY_ESTN_UNIT.NON_ZERO_PLOTS) NON_ZERO_PLOTS,
       SUM(ESTIMATE_BY_ESTN_UNIT.TOT_POP_AREA_ACRES) TOT_POP_AC,
       ' ' FILTER,
       ' ' JOINS
  FROM (SELECT POP_EVAL_GRP_CN,
               EVAL_GRP,
               EVAL_GRP_DESCR,
               SUM(COALESCE(YSUM_HD, 0) * PHASE_1_SUMMARY.EXPNS) ESTIMATE,
               PHASE_1_SUMMARY.N TOTAL_PLOTS,
               SUM(PHASE_SUMMARY.NUMBER_PLOTS_IN_DOMAIN) DOMAIN_PLOTS,
               SUM(PHASE_SUMMARY.NON_ZERO_PLOTS) NON_ZERO_PLOTS,
               TOTAL_AREA * TOTAL_AREA / PHASE_1_SUMMARY.N *
               ((SUM(W_H * PHASE_1_SUMMARY.N_H *
                     (((COALESCE(YSUM_HD_SQR, 0) / PHASE_1_SUMMARY.N_H) -
                     ((COALESCE(YSUM_HD, 0) / PHASE_1_SUMMARY.N_H) *
                     (COALESCE(YSUM_HD, 0) / PHASE_1_SUMMARY.N_H))) /
                     (PHASE_1_SUMMARY.N_H - 1)))) +
               1 / PHASE_1_SUMMARY.N *
               (SUM((1 - W_H) * PHASE_1_SUMMARY.N_H *
                     (((COALESCE(YSUM_HD_SQR, 0) / PHASE_1_SUMMARY.N_H) -
                     ((COALESCE(YSUM_HD, 0) / PHASE_1_SUMMARY.N_H) *
                     (COALESCE(YSUM_HD, 0) / PHASE_1_SUMMARY.N_H))) /
                     (PHASE_1_SUMMARY.N_H - 1))))) VAR_OF_ESTIMATE,
               TOTAL_AREA TOT_POP_AREA_ACRES,
               GRP_BY_ATTRIB
          FROM (SELECT PEV.CN EVAL_CN,
                       PEG.EVAL_GRP,
                       PEG.EVAL_GRP_DESCR,
                       PEG.CN POP_EVAL_GRP_CN,
                       POP_STRATUM.ESTN_UNIT_CN,
                       POP_STRATUM.EXPNS,
                       POP_STRATUM.CN POP_STRATUM_CN,
                       P1POINTCNT /
                       (SELECT SUM(STR.P1POINTCNT)
                          FROM FS_FIADB.POP_STRATUM STR
                         WHERE STR.ESTN_UNIT_CN = POP_STRATUM.ESTN_UNIT_CN) W_H,
                       (SELECT SUM(STR.P1POINTCNT)
                          FROM FS_FIADB.POP_STRATUM STR
                         WHERE STR.ESTN_UNIT_CN = POP_STRATUM.ESTN_UNIT_CN) N_PRIME,
                       P1POINTCNT N_PRIME_H,
                       (SELECT SUM(EU_S.AREA_USED)
                          FROM FS_FIADB.POP_ESTN_UNIT EU_S
                         WHERE EU_S.CN = POP_STRATUM.ESTN_UNIT_CN) TOTAL_AREA,
                       (SELECT SUM(STR.P2POINTCNT)
                          FROM FS_FIADB.POP_STRATUM STR
                         WHERE STR.ESTN_UNIT_CN = POP_STRATUM.ESTN_UNIT_CN) N,
                       POP_STRATUM.P2POINTCNT N_H
                  FROM FS_FIADB.POP_EVAL_GRP PEG
                  JOIN FS_FIADB.POP_EVAL_TYP PET
                    ON (PET.EVAL_GRP_CN = PEG.CN)
                  JOIN FS_FIADB.POP_EVAL PEV
                    ON (PEV.CN = PET.EVAL_CN)
                  JOIN FS_FIADB.POP_ESTN_UNIT PEU
                    ON (PEV.CN = PEU.EVAL_CN)
                  JOIN FS_FIADB.POP_STRATUM POP_STRATUM
                    ON (PEU.CN = POP_STRATUM.ESTN_UNIT_CN)
                 WHERE PEG.EVAL_GRP IN (12018,
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
                   AND PET.EVAL_TYP = 'EXPREMV') PHASE_1_SUMMARY
          LEFT OUTER JOIN (SELECT POP_STRATUM_CN,
                                 ESTN_UNIT_CN,
                                 EVAL_CN,
                                 SUM(Y_HID_ADJUSTED) YSUM_HD,
                                 SUM(Y_HID_ADJUSTED * Y_HID_ADJUSTED) YSUM_HD_SQR,
                                 COUNT(*) NUMBER_PLOTS_IN_DOMAIN,
                                 SUM(CASE
                                       WHEN Y_HID_ADJUSTED IS NULL THEN
                                        0
                                       WHEN Y_HID_ADJUSTED = 0 THEN
                                        0
                                       ELSE
                                        1
                                     END) NON_ZERO_PLOTS,
                                 GRP_BY_ATTRIB
                            FROM (SELECT SUM((TREE.FREMVCFAL *
                                             TREE.TPAREMV_UNADJ * CASE
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
                                                      CASE
                                                       LEAST(TREE.DIA,
                                                             COALESCE(PLOT.MACRO_BREAKPOINT_DIA,
                                                                      9999) - 0.001)
                                                        WHEN TREE.DIA THEN
                                                         POP_STRATUM.ADJ_FACTOR_SUBP
                                                        ELSE
                                                         POP_STRATUM.ADJ_FACTOR_MACR
                                                      END
                                                   END
                                                END
                                             END)) AS Y_HID_ADJUSTED,
                                         PEU.CN ESTN_UNIT_CN,
                                         PEV.CN EVAL_CN,
                                         POP_STRATUM.CN POP_STRATUM_CN,
                                         PLOT.CN PLT_CN,
                                         PCOND.STDSZCD GRP_BY_ATTRIB
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
                                      ON (POP_PLOT_STRATUM_ASSGN.STRATUM_CN =
                                         POP_STRATUM.CN)
                                    JOIN FS_FIADB.PLOT PLOT
                                      ON (POP_PLOT_STRATUM_ASSGN.PLT_CN =
                                         PLOT.CN)
                                    JOIN FS_FIADB.PLOTGEOM PLOTGEOM
                                      ON (PLOT.CN = PLOTGEOM.CN)
                                    JOIN FS_FIADB.SDS_COND_VW COND
                                      ON (COND.PLT_CN = PLOT.CN)
                                    JOIN FS_FIADB.TREE TREE
                                      ON (TREE.PLT_CN = PLOT.CN AND
                                         TREE.CONDID = COND.CONDID)
                                    JOIN FS_FIADB.TREE_GRM_ESTN GRM
                                      ON (TREE.CN = GRM.TRE_CN)
                                    JOIN FS_FIADB.PLOT PPLOT
                                      ON (PPLOT.CN = PLOT.PREV_PLT_CN)
                                    JOIN FS_FIADB.SDS_COND_VW PCOND
                                      ON (PLOT.PREV_PLT_CN = PCOND.PLT_CN AND
                                         TREE.PREVCOND = PCOND.CONDID)
                                    LEFT OUTER JOIN FS_FIADB.TREE PTREE
                                      ON (TREE.PREV_TRE_CN = PTREE.CN)
                                   WHERE GRM.ESTIMATE = 'VOLUME'
                                     AND GRM.ESTN_TYPE = 'AL'
                                     AND GRM.ESTN_UNITS = 'CF'
                                     AND GRM.LAND_BASIS = 'FORESTLAND'
                                     AND TREE.TPAREMV_UNADJ IS NOT NULL
                                     AND TREE.FREMVCFAL IS NOT NULL
                                     AND PET.EVAL_TYP = 'EXPREMV'
                                     AND PEG.EVAL_GRP IN
                                         (12018,
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
                                     AND 1 = 1
                                   GROUP BY PEU.CN,
                                            PEV.CN,
                                            POP_STRATUM.CN,
                                            PLOT.CN,
                                            PCOND.STDSZCD) PLOT_SUMMARY
                           GROUP BY POP_STRATUM_CN,
                                    ESTN_UNIT_CN,
                                    EVAL_CN,
                                    GRP_BY_ATTRIB) PHASE_SUMMARY
            ON (PHASE_1_SUMMARY.POP_STRATUM_CN =
               PHASE_SUMMARY.POP_STRATUM_CN AND
               PHASE_1_SUMMARY.EVAL_CN = PHASE_SUMMARY.EVAL_CN AND
               PHASE_1_SUMMARY.ESTN_UNIT_CN = PHASE_SUMMARY.ESTN_UNIT_CN)
         GROUP BY PHASE_1_SUMMARY.POP_EVAL_GRP_CN,
                  PHASE_1_SUMMARY.EVAL_GRP,
                  PHASE_1_SUMMARY.EVAL_GRP_DESCR,
                  PHASE_1_SUMMARY.ESTN_UNIT_CN,
                  PHASE_1_SUMMARY.TOTAL_AREA,
                  PHASE_1_SUMMARY.N,
                  GRP_BY_ATTRIB) ESTIMATE_BY_ESTN_UNIT
 WHERE NON_ZERO_PLOTS IS NOT NULL
 GROUP BY POP_EVAL_GRP_CN,
          EVAL_GRP,
          EVAL_GRP_DESCR,
          COALESCE(CAST(GRP_BY_ATTRIB AS VARCHAR(4000)), 'NOT AVAILABLE')
