require File.dirname(__FILE__) + '/../../spec_helper'

as_superuser do
  describe "File.chown" do
    before :each do
      @fname = tmp('file_chown_test')
      File.open(@fname, 'w') { }
    end

    after :each do
      File.delete @fname if File.exist? @fname
    end

    platform_is :windows do
      it "does not modify the owner id of the file" do
        File.chown 0, nil, @fname
        File.stat(@fname).uid.should == 0
        File.chown 501, nil, @fname
        File.stat(@fname).uid.should == 0
      end

      it "does not modify the group id of the file" do
        File.chown nil, 0, @fname
        File.stat(@fname).gid.should == 0
        File.chown nil, 501, @fname
        File.stat(@fname).gid.should == 0
      end
    end

    platform_is_not :windows do
      it "changes the owner id of the file" do
        File.chown 501, nil, @fname
        File.stat(@fname).uid.should == 501
        File.chown 0, nil, @fname
        File.stat(@fname).uid.should == 0
      end

      it "changes the group id of the file" do
        File.chown nil, 501, @fname
        File.stat(@fname).gid.should == 501
        File.chown nil, 0, @fname
        File.stat(@fname).uid.should == 0
      end

      it "does not modify the owner id of the file if passed nil or -1" do
        File.chown 501, nil, @fname
        File.chown nil, nil, @fname
        File.stat(@fname).uid.should == 501
        File.chown nil, -1, @fname
        File.stat(@fname).uid.should == 501
      end

      it "does not modify the group id of the file if passed nil or -1" do
        File.chown nil, 501, @fname
        File.chown nil, nil, @fname
        File.stat(@fname).gid.should == 501
        File.chown nil, -1, @fname
        File.stat(@fname).gid.should == 501
      end

      it "returns the number of files processed" do
        File.chown(nil, nil, @fname, @fname).should == 2
      end
    end
  end

  describe "File#chown" do
    before :each do
      @fname = tmp('file_chown_test')
      @file = File.open(@fname, 'w')
    end

    after :each do
      @file.close unless @file.closed?
      File.delete @fname if File.exist? @fname
    end

    platform_is :windows do
      it "does not modify the owner id of the file" do
        File.chown 0, nil, @fname
        File.stat(@fname).uid.should == 0
        File.chown 501, nil, @fname
        File.stat(@fname).uid.should == 0
      end

      it "does not modify the group id of the file" do
        File.chown nil, 0, @fname
        File.stat(@fname).gid.should == 0
        File.chown nil, 501, @fname
        File.stat(@fname).gid.should == 0
      end
    end

    platform_is_not :windows do
      it "changes the owner id of the file" do
        @file.chown 501, nil
        @file.stat.uid.should == 501
        @file.chown 0, nil
        @file.stat.uid.should == 0
      end

      it "changes the group id of the file" do
        @file.chown nil, 501
        @file.stat.gid.should == 501
        @file.chown nil, 0
        @file.stat.uid.should == 0
      end

      it "does not modify the owner id of the file if passed nil or -1" do
        @file.chown 501, nil
        @file.chown nil, nil
        @file.stat.uid.should == 501
        @file.chown nil, -1
        @file.stat.uid.should == 501
      end

      it "does not modify the group id of the file if passed nil or -1" do
        @file.chown nil, 501
        @file.chown nil, nil
        @file.stat.gid.should == 501
        @file.chown nil, -1
        @file.stat.gid.should == 501
      end
    end

    it "returns 0" do
      @file.chown(nil, nil).should == 0
    end
  end
end
