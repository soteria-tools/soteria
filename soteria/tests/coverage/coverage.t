Coverage report can be exported to JSON and Cobertura XML
  $ ./print_coverage.exe
  {
    "sample.c": {
      "lines": { "3": 2, "4": 1 },
      "branches": {
        "tests/sample-branch": {
          "line": 7,
          "then_reached": true,
          "else_reached": true
        }
      }
    }
  }
  
  <?xml version="1.0" ?>
  <coverage lines-valid="2" lines-covered="2" line-rate="1.000000" branches-valid="2" branches-covered="2" branch-rate="1.000000" version="soteria">
    <packages>
      <package name="soteria" line-rate="1.000000" branch-rate="1.000000">
        <classes>
          <class name="sample.c" filename="sample.c" line-rate="0.0" branch-rate="0.0">
            <methods/>
            <lines>
              <line number="3" hits="2"/>
              <line number="4" hits="1"/>
              <line number="7" hits="1" branch="true" condition-coverage="100% (2/2)"/>
            </lines>
          </class>
        </classes>
      </package>
    </packages>
  </coverage>
  
  {
    "sample.c": {
      "lines": { "3": 2, "4": 1 },
      "branches": {
        "tests/sample-branch": {
          "line": 7,
          "then_reached": true,
          "else_reached": true
        }
      }
    }
  }
