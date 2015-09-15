cf rename ownpt2 ownpt2_old && \
    cf push ownpt2 && \
    cf delete ownpt2_old -f 
