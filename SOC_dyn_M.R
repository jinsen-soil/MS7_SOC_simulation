# this is an example of the model structure constructed for Mbeya site.
# for the details of the conceptual model please refer to Clivot et al. 2019.

SOC.dyn.M <- function(time, stocks, parms) {
  with(as.list(c(stocks, parms)), {
    # define fluxes
    
    # C input through root
    root_input.0N <- avg.root_in_M.0N # root biomass calculated with field-measured shoot-root ratio and above-ground biomass
    root_input.50N <- avg.root_in_M.50N
    root_input.50NR <- avg.root_in_M.50NR
    root_input.100N <- avg.root_in_M.100N
    root_input.150N <- avg.root_in_M.150N
    root_input.150NR <- avg.root_in_M.150NR
    
    # C input through rhizodeposition
    rhizod_C.0N <- 0.65*root_input.0N*0.8  # rhizod_C assumed to be 0.65 times root biomass; 0.8 is for depth correction in Mbeya site.
    rhizod_C.50N <- 0.65*root_input.50N*0.8
    rhizod_C.50NR <- 0.65*root_input.50NR*0.8
    rhizod_C.100N <- 0.65*root_input.100N*0.8
    rhizod_C.150N <- 0.65*root_input.150N*0.8
    rhizod_C.150NR <- 0.65*root_input.150NR*0.8
    
    
    Cbg_in.0N = (root_input.0N+rhizod_C.0N)*h_bg  # belowground C input humified into the active C pool  
    Cbg_in.50N = (root_input.50N+rhizod_C.50N)*h_bg
    Cbg_in.50NR = (root_input.50NR+rhizod_C.50NR)*h_bg
    Cbg_in.100N = (root_input.100N+rhizod_C.100N)*h_bg
    Cbg_in.150N = (root_input.150N+rhizod_C.150N)*h_bg
    Cbg_in.150NR = (root_input.150NR+rhizod_C.150NR)*h_bg
    
    # C input through residue application for NR treat only
    Cres_in = m_M*h_M
    
    # C out put through SOCa decomposition
    Cout.0N = SOCa_M.0N*k_M
    Cout.50N = SOCa_M.50N*k_M
    Cout.50NR = SOCa_M.50NR*k_M
    Cout.100N = SOCa_M.100N*k_M
    Cout.150N = SOCa_M.150N*k_M
    Cout.150NR = SOCa_M.150NR*k_M
    
    # define the number of ddt corresponds to the number of stocks
    ddt = rep(0, length(stock_M))
    
    # 0N pool
    ddt[1] <- Cbg_in.0N - Cout.0N # active pool
    ddt[2] <- 0 # stable pool
    
    # 50N pool
    ddt[3] <- Cbg_in.50N - Cout.50N # active pool
    ddt[4] <- 0 # stable pool
    
    # 50NR pool
    ddt[5] <- Cbg_in.50NR + Cres_in - Cout.50NR
    ddt[6] <- 0
    
    # 100N pool
    ddt[7] <- Cbg_in.100N - Cout.100N # active pool
    ddt[8] <- 0
    
    # 150N pool
    ddt[9] <- Cbg_in.150N - Cout.150N # active pool
    ddt[10] <- 0 # stable pool
    
    # 150NR pool
    ddt[11] <- Cbg_in.150NR + Cres_in - Cout.150NR # active pool
    ddt[12] <- 0 # stable pool
    
    deltaSOCa.0N = Cbg_in.0N - Cout.0N
    deltaSOCa.50N = Cbg_in.50N - Cout.50N
    deltaSOCa.50NR = Cbg_in.50NR+Cres_in - Cout.50NR
    deltaSOCa.100N = Cbg_in.100N - Cout.100N
    deltaSOCa.150N = Cbg_in.150N - Cout.150N
    deltaSOCa.150NR = Cbg_in.150NR+Cres_in - Cout.150NR
    
    SOCt.0N  = SOCa_M.0N + SOCs_M.0N
    SOCt.50N  = SOCa_M.50N + SOCs_M.50N
    SOCt.50NR = SOCa_M.50NR + SOCs_M.50NR
    SOCt.100N  = SOCa_M.100N + SOCs_M.100N
    SOCt.150N  = SOCa_M.150N + SOCs_M.150N
    SOCt.150NR = SOCa_M.150NR + SOCs_M.150NR
    
    return(list(ddt,
                SOCt.0N = SOCt.0N,
                SOCt.50N = SOCt.50N,
                SOCt.50NR = SOCt.50NR,
                SOCt.100N = SOCt.100N,
                SOCt.150N = SOCt.150N,
                SOCt.150NR = SOCt.150NR,
                deltaSOCa.0N = deltaSOCa.0N,                
                deltaSOCa.50N = deltaSOCa.50N,
                deltaSOCa.50NR = deltaSOCa.50NR,
                deltaSOCa.100N = deltaSOCa.100N, 
                deltaSOCa.150N = deltaSOCa.150N,
                deltaSOCa.150NR = deltaSOCa.150NR
    ))
    
    
  })
}
