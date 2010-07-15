require File.dirname(__FILE__) + '/../../spec_helper'

as_superuser do
  describe "File.lchown" do
    not_supported_on :windows do
      before :each do
        @fname = tmp('file_chown_test')
        @lname = @fname + '.lnk'
        File.delete @fname rescue nil
        File.delete @lname rescue nil
        File.open(@fname, 'w') { |f| f.chown 501, 501 }
        File.symlink @fname, @lname
      end
  
      after :each do
        File.delete @fname if File.exist? @fname
        File.delete @lname if File.exist? @lname
      end
  
      it "changes the owner id of the file" do
        File.lchown 502, nil, @lname
        File.stat(@fname).uid.should == 501
        File.lstat(@lname).uid.should == 502
        File.lchown 0, nil, @lname
        File.stat(@fname).uid.should == 501
        File.lstat(@lname).uid.should == 0
      end
  
      it "changes the group id of the file" do
        File.lchown nil, 502, @lname
        File.stat(@fname).gid.should == 501
        File.lstat(@lname).gid.should == 502
        File.lchown nil, 0, @lname
        File.stat(@fname).uid.should == 501
        File.lstat(@lname).uid.should == 0
      end
  
      it "does not modify the owner id of the file if passed nil or -1" do
        File.lchown 502, nil, @lname
        File.lchown nil, nil, @lname
        File.lstat(@lname).uid.should == 502
        File.lchown nil, -1, @lname
        File.lstat(@lname).uid.should == 502
      end
  
      it "does not modify the group id of the file if passed nil or -1" do
        File.lchown nil, 502, @lname
        File.lchown nil, nil, @lname
        File.lstat(@lname).gid.should == 502
        File.lchown nil, -1, @lname
        File.lstat(@lname).gid.should == 502
      end
  
      it "returns the number of files processed" do
        File.lchown(nil, nil, @lname, @lname).should == 2
      end
    end
  end
end
