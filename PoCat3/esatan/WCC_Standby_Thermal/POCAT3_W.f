      PROGRAM POCAT3_WCC_STANDBY_THERM                                          
C   
      INCLUDE 'POCAT3_W.h'                                                      
C   
      CHARACTER MNAME * 24                                                      
C   
      MNAME = 'POCAT3_WCC_STANDBY_THERM'                                        
C   
      FLG(1) = 2136                                                             
      FLG(2) = 3327                                                             
      FLG(3) = 0                                                                
      FLG(4) = 34187                                                            
      FLG(5) = 0                                                                
      FLG(6) = 1                                                                
      FLG(7) = 0                                                                
      FLG(8) = 2                                                                
      FLG(9) = 13                                                               
      FLG(10) = 0                                                               
      FLG(11) = 2                                                               
      FLG(12) = 0                                                               
      FLG(13) = 49                                                              
      FLG(14) = 25                                                              
      FLG(15) = 16                                                              
      FLG(16) = 14                                                              
      FLG(17) = 92                                                              
      FLG(18) = 22                                                              
      FLG(19) = 2                                                               
      FLG(20) = 34                                                              
      FLG(21) = 0                                                               
      FLG(22) = 0                                                               
      FLG(23) = 0                                                               
      FLG(24) = 162894                                                          
      FLG(25) = 2                                                               
      FLG(26) = 1                                                               
      FLG(27) = 133                                                             
      FLG(28) = 1067418                                                         
      FLG(29) = 1                                                               
      FLG(30) = 0                                                               
      FLG(31) = 0                                                               
      FLG(32) = 0                                                               
      FLG(33) = 0                                                               
      FLG(34) = 0                                                               
      FLG(35) = 22                                                              
      FLG(36) = 1                                                               
      FLG(37) = 0                                                               
      FLG(38) = 0                                                               
      FLG(39) = 68374                                                           
      FLG(40) = 0                                                               
      FLG(41) = 0                                                               
      CALL SVMNAM(MNAME)                                                        
C   
      SPRNDM = 1                                                                
      SPRNDN = 3                                                                
      USRNDC = 0                                                                
      USRNDI = 0                                                                
      USRNDR = 0                                                                
C   
      CALL MAINA(MNAME)                                                         
C   
      CALL SUCCES                                                               
C   
      STOP                                                                      
C   
      END                                                                       
      SUBROUTINE IN0001                                                 
      INCLUDE 'POCAT3_W.h'
      LOGICAL HTFLAG                                                    
      HTFLAG = .TRUE.                                                   
      OPBLOK = 'INITIAL   '                                             
      IG(2) = 1                                                         
      CALL GM0001(HTFLAG)                                               
      CALL FINITS(' ' , -    1)                                         
      QI(MD(3,1)+1286)=1.D-05                                           
      QI(MD(3,1)+1)=1.D-05                                              
      QI(MD(3,1)+2)=1.D-05                                              
      QI(MD(3,1)+4)=0.D+00                                              
      QI(MD(3,1)+1285)=0.D+00                                           
      QI(MD(3,1)+1284)=2.D-04                                           
      QI(MD(3,1)+1282)=2.4D-04                                          
      QI(MD(3,1)+1281)=1.5D-04                                          
      QI(MD(3,1)+1280)=0.D+00                                           
      QI(MD(3,1)+1279)=0.D+00                                           
      QI(MD(3,1)+1278)=0.D+00                                           
      QI(MD(3,1)+1277)=0.D+00                                           
      QI(MD(3,1)+1276)=4.4D-02                                          
      QI(MD(3,1)+1275)=1.452D-01                                        
      QI(MD(3,1)+1466)=5.D-01                                           
      OPBLOK = ' '                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE V10001                                                 
      INCLUDE 'POCAT3_W.h'
      LOGICAL HTFLAG                                                    
      HTFLAG = (SOLTYP .EQ. 'THERMAL' .OR. SOLTYP .EQ. 'HUMID')         
      OPBLOK = 'VARIABLES1'                                             
      IG(2) = 1                                                         
      CALL PRMUPD                                                       
      CALL CHKTRM                                                       
      CALL ACDDYU                                                       
      CALL HEATER_UPDATE                                                
      IG(2) = 1                                                         
      QI(MD(3,1)+1286)=1.D-05                                           
      QI(MD(3,1)+1)=1.D-05                                              
      QI(MD(3,1)+2)=1.D-05                                              
      QI(MD(3,1)+4)=0.D+00                                              
      QI(MD(3,1)+1285)=0.D+00                                           
      QI(MD(3,1)+1284)=2.D-04                                           
      QI(MD(3,1)+1282)=2.4D-04                                          
      QI(MD(3,1)+1281)=1.5D-04                                          
      QI(MD(3,1)+1280)=0.D+00                                           
      QI(MD(3,1)+1279)=0.D+00                                           
      QI(MD(3,1)+1278)=0.D+00                                           
      QI(MD(3,1)+1277)=0.D+00                                           
      QI(MD(3,1)+1276)=4.4D-02                                          
      QI(MD(3,1)+1275)=1.452D-01                                        
      QI(MD(3,1)+1466)=5.D-01                                           
      CALL GM0001(HTFLAG)                                               
      OPBLOK = ' '                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE GM0001(HTFLAG)                                         
      LOGICAL HTFLAG                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE V20001                                                 
      INCLUDE 'POCAT3_W.h'
      OPBLOK = 'VARIABLES2'                                             
      IG(2) = 1                                                         
      CALL SSNCNT(FLG(24),FLG(25),MAX0(FLG(1),1),PCS,T)                 
      IG(25) = IG(25) + 1                                               
      CALL PRMUPD                                                       
      CALL HEATER_UPDATE                                                
      IG(2) = 1                                                         
      OPBLOK = ' '                                                      
      CALL PARWRT('VARIABLES2')                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE EXECTN                                                 
      INCLUDE 'POCAT3_W.h'
      IG(4)=100                                                         
      RG(13)=0.01                                                       
      CALL SOLVFM                                                       
      RG(18)=5612.749                                                   
      RG(12)=300.0                                                      
      IG(4)=100                                                         
      RG(13)=0.01                                                       
      RG(3)=100.0                                                       
      CALL SOLCYC('SLCRNC',0.01D0,0.01D0,5612.749D0,100,' ','NONE')     
      CALL SLCRNC                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE OUTPUT                                                 
      INCLUDE 'POCAT3_W.h'
      IF (OUTIME .NE. 'ALL') RETURN                                     
      OPBLOK = 'OUTPUTS'                                                
      CALL PRNDTB(' ','L, T, QS, QE, QA, QI, C',1)                      
      CALL DMPTMD(' ','NODES, CONDUCTORS, CONSTANTS',1,' ')             
      OPBLOK = ' '                                                      
      CALL PARWRT('OUTPUTS')                                            
      RETURN                                                            
      END                                                               
