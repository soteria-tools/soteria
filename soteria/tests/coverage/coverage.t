Coverage report can be exported to JSON, Cobertura XML, and LCOV
  $ ./print_coverage.exe
  {
    "my.file": {
      "lines": { "1": 1, "2": 2, "3": 2, "4": 4, "5": 2, "6": 2, "7": 0 },
      "branches": {
        "b_2_1": { "line": 2, "then_hits": 1, "else_hits": 1 },
        "b_4_1": { "line": 4, "then_hits": 0, "else_hits": 2 },
        "b_4_2": { "line": 4, "then_hits": 2, "else_hits": 0 },
        "b_6_1": { "line": 6, "then_hits": 1, "else_hits": 1 }
      },
      "functions": {
        "my_fun": { "line": 1, "hits": 1 },
        "other_fun": { "line": 15, "hits": 0 }
      }
    }
  }
  
  <?xml version="1.0" ?>
  <coverage lines-valid="7" lines-covered="6" line-rate="0.857143" branches-valid="8" branches-covered="6" branch-rate="0.750000" version="soteria">
    <packages>
      <package name="soteria" line-rate="0.857143" branch-rate="0.750000">
        <classes>
          <class name="my.file" filename="my.file" line-rate="0.0" branch-rate="0.0">
            <methods>
              <method name="my_fun" signature="" line-rate="1.0" branch-rate="0.0">
                <lines>
                  <line number="1" hits="1"/>
                </lines>
              </method>
              <method name="other_fun" signature="" line-rate="0.0" branch-rate="0.0">
                <lines>
                  <line number="15" hits="0"/>
                </lines>
              </method>
            </methods>
            <lines>
              <line number="1" hits="1"/>
              <line number="2" hits="2" branch="true" condition-coverage="100% (2/2)"/>
              <line number="3" hits="2"/>
              <line number="4" hits="4" branch="true" condition-coverage="50% (2/4)"/>
              <line number="5" hits="2"/>
              <line number="6" hits="2" branch="true" condition-coverage="100% (2/2)"/>
              <line number="7" hits="0"/>
            </lines>
          </class>
        </classes>
      </package>
    </packages>
  </coverage>
  
  SF:my.file
  FN:1,my_fun
  FNDA:1,my_fun
  FN:15,other_fun
  FNDA:0,other_fun
  FNF:2
  FNH:1
  BRDA:2,0,then,1
  BRDA:2,0,else,1
  BRDA:4,0,then,0
  BRDA:4,0,else,2
  BRDA:4,1,then,2
  BRDA:4,1,else,0
  BRDA:6,0,then,1
  BRDA:6,0,else,1
  DA:1,1
  DA:2,2
  DA:3,2
  DA:4,4
  DA:5,2
  DA:6,2
  DA:7,0
  BRF:8
  BRH:6
  LF:7
  LH:6
  end_of_record
  
