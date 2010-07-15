require 'logger'

module LoggerSpecs

  def self.strip_date(str)
    str.gsub(/[A-Z].*\[.*\]/, "").lstrip
  end
  class TestApp < Logger::Application
    def initialize(appname, log_file=nil)
      super(appname)
      self.set_log(log_file) if log_file
    end

    def run
      log(WARN, "Test log message")
    end
  end
end
