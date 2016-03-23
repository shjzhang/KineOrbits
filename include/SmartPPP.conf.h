c 
c   Estimator: lsq or kal
c 
      character*10  estimator 
c
c   observation model
c
      integer       observ_model
c
c   Tropospheric model: Hopfield, Saastamoinen
c
      character*20  trop_model 
c
c   Mapping function of Tropospheric: Neil
c
      character*20  trop_mapp_func 
c
c++ Configure parameters for Batch Least Square 
c
c   The estimation interval for tropospheric in batch least square
c
      real*8        trop_estmt_intv
c
c   tropospheric estimation function in batch least square: piece_constant, piece linear
c
      character*20  trop_estmt_func 
c
c++ Configure parameters for Kalman Filter
c
c   A-Priori Standard deviation for Kal [m]
c
      real*8        sigma_pos_kal
c
c   A-Priori Standard deviation for Kal [m]
c
      real*8        sigma_vel_kal 
c
c   process noise for clock: mm/h^(1/2)
c
      real*8        process_noise_clock 
c
c   random walk process noise: mm/h^(1/2)
c
      real*8        process_noise_trop 
c
      common /SmartPPP_conf/ sigma_pos_kal,sigma_vel_kal, 
     &                       process_noise_clock,process_noise_trop,
     &                       trop_estmt_intv,trop_estmt_func,
     &                       trop_mapp_func,trop_model,observ_model,
     &                       estimator                       
c
c   END
c

