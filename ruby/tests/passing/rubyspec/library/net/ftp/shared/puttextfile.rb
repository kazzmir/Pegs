describe :net_ftp_puttextfile, :shared => true do
  before(:each) do
    @server = NetFTPSpecs::DummyFTP.new
    @server.serve_once

    @local_fixture_file  = File.dirname(__FILE__) + "/../fixtures/puttextfile"
    @remote_tmp_file = tmp("textfile")

    @ftp = Net::FTP.new
    @ftp.connect("localhost", 9921)
    @ftp.binary = @binary_mode
  end

  after(:each) do
    @ftp.quit rescue nil
    @ftp.close
    @server.stop
  end

  after :all do
    File.unlink(tmp("textfile")) if File.exists?(tmp("textfile"))
  end

  it "sends the STOR command to the server" do
    @ftp.send(@method, @local_fixture_file, "text")
    @ftp.last_response.should == "200 OK, Data received. (STOR text)\n"
  end
  
  it "sends the contents of the passed local_file, using \\r\\n as the newline seperator" do
    @ftp.send(@method, @local_fixture_file, "text")
    
    remote_lines = File.readlines(@remote_tmp_file)
    local_lines  = File.readlines(@local_fixture_file)
    
    remote_lines.should_not == local_lines
    remote_lines.should == local_lines.map { |l| l.chomp + "\r\n" }
  end

  it "returns nil" do
    @ftp.send(@method, @local_fixture_file, "text").should be_nil
  end
  
  describe "when passed a block" do
    it "yields each transmitted line" do
      res = []
      @ftp.send(@method, @local_fixture_file, "text") { |x| res << x }
      res.should == [
        "This is an example file\r\n",
        "which is going to be transmitted\r\n",
        "using #puttextfile.\r\n"
      ]
    end
  end

  describe "when the STOR command fails" do
    it "raises a Net::FTPPermError when the response code is 532" do
      @server.should_receive(:stor).and_respond("532 Need account for storing files.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPPermError)
    end

    it "raises a Net::FTPTempError when the response code is 450" do
      @server.should_receive(:stor).and_respond("450 Requested file action not taken.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPTempError)
    end
    
    it "raises a Net::FTPTempError when the response code is 452" do
      @server.should_receive(:stor).and_respond("452 Requested action not taken.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPTempError)
    end
    
    it "raises a Net::FTPPermError when the response code is 553" do
      @server.should_receive(:stor).and_respond("553 Requested action not taken.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPPermError)
    end
    
    it "raises a Net::FTPPermError when the response code is 500" do
      @server.should_receive(:stor).and_respond("500 Syntax error, command unrecognized.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPPermError)
    end
    
    it "raises a Net::FTPPermError when the response code is 501" do
      @server.should_receive(:stor).and_respond("501 Syntax error in parameters or arguments.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPPermError)
    end
    
    it "raises a Net::FTPTempError when the response code is 421" do
      @server.should_receive(:stor).and_respond("421 Service not available, closing control connection.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPTempError)
    end
    
    it "raises a Net::FTPPermError when the response code is 530" do
      @server.should_receive(:stor).and_respond("530 Not logged in.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPPermError)
    end
  end

  describe "when switching type fails" do
    it "raises a Net::FTPPermError when the response code is 500" do
      @server.should_receive(:type).and_respond("500 Syntax error, command unrecognized.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPPermError)
    end
    
    it "raises a Net::FTPPermError when the response code is 501" do
      @server.should_receive(:type).and_respond("501 Syntax error in parameters or arguments.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPPermError)
    end
    
    it "raises a Net::FTPPermError when the response code is 504" do
      @server.should_receive(:type).and_respond("504 Command not implemented for that parameter.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPPermError)
    end
    
    it "raises a Net::FTPTempError when the response code is 421" do
      @server.should_receive(:type).and_respond("421 Service not available, closing control connection.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPTempError)
    end
    
    it "raises a Net::FTPPermError when the response code is 530" do
      @server.should_receive(:type).and_respond("530 Not logged in.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPPermError)
    end
  end
  
  describe "when opening the data port fails" do
    it "raises a Net::FTPPermError when the response code is 500" do
      @server.should_receive(:eprt).and_respond("500 Syntax error, command unrecognized.")
      @server.should_receive(:port).and_respond("500 Syntax error, command unrecognized.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPPermError)
    end

    it "raises a Net::FTPPermError when the response code is 501" do
      @server.should_receive(:eprt).and_respond("501 Syntax error in parameters or arguments.")
      @server.should_receive(:port).and_respond("501 Syntax error in parameters or arguments.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPPermError)
    end

    it "raises a Net::FTPTempError when the response code is 421" do
      @server.should_receive(:eprt).and_respond("421 Service not available, closing control connection.")
      @server.should_receive(:port).and_respond("421 Service not available, closing control connection.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPTempError)
    end
    
    it "raises a Net::FTPPermError when the response code is 530" do
      @server.should_receive(:eprt).and_respond("530 Not logged in.")
      @server.should_receive(:port).and_respond("530 Not logged in.")
      lambda { @ftp.send(@method, @local_fixture_file, "text") }.should raise_error(Net::FTPPermError)
    end
  end
end
