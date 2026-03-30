Coverage report can be exported to JSON, Cobertura XML, and LCOV
  $ ./print_coverage.exe
  {
    "my.file": {
      "lines": { "1": 1, "2": 2, "3": 2, "4": 2, "5": 2, "6": 0, "7": 0 },
      "branches": {
        "b_2_1": { "line": 2, "then_reached": true, "else_reached": true },
        "b_4_1": { "line": 4, "then_reached": false, "else_reached": true },
        "b_4_2": { "line": 4, "then_reached": true, "else_reached": false }
      }
    }
  }
  
  <?xml version="1.0" ?>
  <coverage lines-valid="7" lines-covered="5" line-rate="0.714286" branches-valid="6" branches-covered="4" branch-rate="0.666667" version="soteria">
    <packages>
      <package name="soteria" line-rate="0.714286" branch-rate="0.666667">
        <classes>
          <class name="my.file" filename="my.file" line-rate="0.0" branch-rate="0.0">
            <methods/>
            <lines>
              <line number="1" hits="1"/>
              <line number="2" hits="2" branch="true" condition-coverage="100% (2/2)"/>
              <line number="3" hits="2"/>
              <line number="4" hits="2" branch="true" condition-coverage="50% (2/4)"/>
              <line number="5" hits="2"/>
              <line number="6" hits="0"/>
              <line number="7" hits="0"/>
            </lines>
          </class>
        </classes>
      </package>
    </packages>
  </coverage>
  
  SF:my.file
  BRDA:2,0,then,1
  BRDA:2,0,else,1
  BRDA:4,0,then,0
  BRDA:4,0,else,1
  BRDA:4,1,then,1
  BRDA:4,1,else,0
  DA:1,1
  DA:2,2
  DA:3,2
  DA:4,2
  DA:5,2
  DA:6,0
  DA:7,0
  BRF:6
  BRH:4
  LF:7
  LH:5
  end_of_record
  

