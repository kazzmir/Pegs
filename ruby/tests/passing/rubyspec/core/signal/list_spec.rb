require File.dirname(__FILE__) + '/../../spec_helper'

describe "Signal.list" do
  RUBY18_SIGNALS = %w{
    EXIT
    HUP
    INT
    QUIT
    ILL
    TRAP
    IOT
    ABRT
    EMT
    FPE
    KILL
    BUS
    SEGV
    SYS
    PIPE
    ALRM
    TERM
    URG
    STOP
    TSTP
    CONT
    CHLD
    CLD
    TTIN
    TTOU
    IO
    XCPU
    XFSZ
    VTALRM
    PROF
    WINCH
    USR1
    USR2
    LOST
    MSG
    PWR
    POLL
    DANGER
    MIGRATE
    PRE
    GRANT
    RETRACT
    SOUND
    INFO
  }

  it "doesn't contain other signals than in 1.8" do
    (Signal.list.keys - RUBY18_SIGNALS).should == []
  end

  if Signal.list["CHLD"]
    it "should redefine CLD with CHLD if defined" do
      Signal.list["CLD"].should == Signal.list["CHLD"]
    end
  end

  it "should contain the EXIT key with a value of zero" do
    Signal.list["EXIT"].should == 0
  end
end
