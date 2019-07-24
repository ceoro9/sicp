; When we connect a wire, its signal is set to 0, so u have to call
; action-callback to propogate changes to others gates, it's technically initialization.
; If we dont't do this, simulation is not going to work, because there is no signal
; propogation in the system.

