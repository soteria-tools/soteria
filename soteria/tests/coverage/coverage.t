Coverage report can be exported to JSON and Cobertura XML
  $ ./print_coverage.exe
  {
    "my.file": {
      "lines": { "1": 1, "3": 2, "5": 2, "6": 0, "7": 0 },
      "branches": {
        "b_2_1": { "line": 2, "then_reached": true, "else_reached": true },
        "b_4_1": { "line": 4, "then_reached": false, "else_reached": true },
        "b_4_2": { "line": 4, "then_reached": true, "else_reached": false }
      }
    }
  }
  
  <?xml version="1.0" ?>
  <coverage lines-valid="5" lines-covered="3" line-rate="0.600000" branches-valid="6" branches-covered="4" branch-rate="0.666667" version="soteria">
    <packages>
      <package name="soteria" line-rate="0.600000" branch-rate="0.666667">
        <classes>
          <class name="my.file" filename="my.file" line-rate="0.0" branch-rate="0.0">
            <methods/>
            <lines>
              <line number="1" hits="1"/>
              <line number="3" hits="2"/>
              <line number="5" hits="2"/>
              <line number="6" hits="0"/>
              <line number="7" hits="0"/>
              <line number="2" hits="1" branch="true" condition-coverage="100% (2/2)"/>
              <line number="4" hits="1" branch="true" condition-coverage="50% (2/4)"/>
            </lines>
          </class>
        </classes>
      </package>
    </packages>
  </coverage>
