***********************************************************************
* KIKSITLG - SYSTEM INITIALIZATION TABLE (SIT) OF THE USAGE LEDGER
* ONLINE. KICKS LOADS IT AS KIKSITLG WHEN STARTED WITH SIT(LG) (SEE
* CLIST LEDGER). IT NAMES THE TABLES OF THIS APPLICATION (LG) AND THE
* FIRST TRANSACTION. THE REST ARE KICKS'S OWN MODULES (1$), AND THE
* TERMINAL DRIVER FOR MVS 3.8 (2$).
***********************************************************************
         PRINT GEN
         KIKSIT SUFFIX=LG,                                             *
               PCT=LG,PPT=LG,FCT=LG,                                   *
               PCP=1$,KCP=1$,FCP=1$,DCP=1$,DCT=1$,                     *
               TCP=2$,BMS=1$,SCP=1$,TSP=1$,                            *
               PLTPI=LLST,FFREEKB=NO
         LTORG
         END
