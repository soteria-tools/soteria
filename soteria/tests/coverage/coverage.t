Coverage report can be exported to JSON, Cobertura XML, and LCOV
  $ ./print_coverage.exe
  {
    "per_file": {},
    "per_function": {
      "my.file:other_fun": {
        "lines": { "16": 0, "17": 0, "18": 0, "15": 0 },
        "branches": {},
        "meta": { "line": 15, "end_line": null }
      },
      "my.file:my_fun": {
        "lines": { "6": 0, "2": 0, "7": 0, "3": 2, "5": 2, "4": 0, "1": 1 },
        "branches": {
          "b_2_1": { "line": 2, "then_hits": 1, "else_hits": 1 },
          "b_4_2": { "line": 4, "then_hits": 2, "else_hits": 0 },
          "b_6_1": { "line": 6, "then_hits": 1, "else_hits": 1 },
          "b_4_1": { "line": 4, "then_hits": 0, "else_hits": 2 }
        },
        "meta": { "hits": 1, "line": 1, "end_line": null }
      }
    }
  }
  
  <?xml version="1.0" ?>
  <coverage lines-valid="11" lines-covered="6" line-rate="0.545455" branches-valid="8" branches-covered="6" branch-rate="0.750000" version="soteria">
    <packages>
      <package name="soteria" line-rate="0.545455" branch-rate="0.750000">
        <classes>
          <class name="my.file" filename="my.file" line-rate="0.0" branch-rate="0.0">
            <methods>
              <method name="my_fun" signature="" line-rate="0.857143" branch-rate="0.750000" complexity="0.0">
                <lines>
                  <line number="1" hits="1"/>
                  <line number="2" hits="2" branch="true" condition-coverage="100% (2/2)"/>
                  <line number="3" hits="2"/>
                  <line number="4" hits="4" branch="true" condition-coverage="50% (2/4)"/>
                  <line number="5" hits="2"/>
                  <line number="6" hits="2" branch="true" condition-coverage="100% (2/2)"/>
                  <line number="7" hits="0"/>
                </lines>
              </method>
              <method name="other_fun" signature="" line-rate="0.000000" branch-rate="1.000000" complexity="0.0">
                <lines>
                  <line number="15" hits="0"/>
                  <line number="16" hits="0"/>
                  <line number="17" hits="0"/>
                  <line number="18" hits="0"/>
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
              <line number="15" hits="0"/>
              <line number="16" hits="0"/>
              <line number="17" hits="0"/>
              <line number="18" hits="0"/>
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
  DA:15,0
  DA:16,0
  DA:17,0
  DA:18,0
  BRF:8
  BRH:6
  LF:11
  LH:6
  end_of_record
  
