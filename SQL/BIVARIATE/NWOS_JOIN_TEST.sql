SELECT NWOS_CERT.PLT_CN,
       CERT,
       COST,
       TAX,
       CASE
         WHEN CERT = 1 OR COST = 1 OR TAX = 1 THEN
          1
         WHEN CERT IN (0, 9) OR COST IN (0, 9) OR TAX IN (0, 9) THEN
          0
         ELSE
          -1
       END AS PROGRAM
  FROM (SELECT PLT_CN, RESPONSE_VALUE CERT
          FROM FS_NWOS.PLOT_OWNER PO,
               FS_NWOS.OWNER      O,
               FS_NWOS.SAMPLE     S,
               FS_NWOS.RESPONSE   R,
               FS_NWOS.QUEST      Q
         WHERE PO.OWNER_CN = O.CN
           AND S.OWNER_CN = O.CN
           AND R.SAMPLE_CN = S.CN
           AND Q.RESPONSE_CN = R.CN
           AND S.NWOS_STUDY = 'base'
           AND PO.ORIGIN = 'P2'
           AND O.OWNCD_NWOS = 45
           AND R.RESPONSE_CD = 1
           AND Q.RESPONSE_VALUE <> -1
           AND Q.METADATA_CN = 'QMD114'
         GROUP BY PLT_CN, RESPONSE_VALUE) NWOS_CERT
  JOIN (SELECT PLT_CN, RESPONSE_VALUE COST
          FROM FS_NWOS.PLOT_OWNER PO,
               FS_NWOS.OWNER      O,
               FS_NWOS.SAMPLE     S,
               FS_NWOS.RESPONSE   R,
               FS_NWOS.QUEST      Q
         WHERE PO.OWNER_CN = O.CN
           AND S.OWNER_CN = O.CN
           AND R.SAMPLE_CN = S.CN
           AND Q.RESPONSE_CN = R.CN
           AND S.NWOS_STUDY = 'base'
           AND PO.ORIGIN = 'P2'
           AND O.OWNCD_NWOS = 45
           AND R.RESPONSE_CD = 1
           AND Q.RESPONSE_VALUE <> -1
           AND Q.METADATA_CN = 'QMD121'
         GROUP BY PLT_CN, RESPONSE_VALUE) NWOS_COST
    ON (NWOS_CERT.PLT_CN = NWOS_COST.PLT_CN)
  JOIN (SELECT PLT_CN, RESPONSE_VALUE TAX
          FROM FS_NWOS.PLOT_OWNER PO,
               FS_NWOS.OWNER      O,
               FS_NWOS.SAMPLE     S,
               FS_NWOS.RESPONSE   R,
               FS_NWOS.QUEST      Q
         WHERE PO.OWNER_CN = O.CN
           AND S.OWNER_CN = O.CN
           AND R.SAMPLE_CN = S.CN
           AND Q.RESPONSE_CN = R.CN
           AND S.NWOS_STUDY = 'base'
           AND PO.ORIGIN = 'P2'
           AND O.OWNCD_NWOS = 45
           AND R.RESPONSE_CD = 1
           AND Q.RESPONSE_VALUE <> -1
           AND Q.METADATA_CN = 'QMD116'
         GROUP BY PLT_CN, RESPONSE_VALUE) NWOS_TAX
    ON (NWOS_CERT.PLT_CN = NWOS_TAX.PLT_CN)
