PROGRAM main

    USE timeloop_mod, ONLY: init_timeloop, finish_timeloop, timeloop

    ! This initialize the time loop. Reads the RUNINFO table in case of DCONT.
    CALL init_timeloop()

    ! Run time loop
    CALL timeloop()

    CALL finish_timeloop()

END PROGRAM main
