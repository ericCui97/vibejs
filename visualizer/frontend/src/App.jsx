import { useState, useRef, useEffect } from 'react';
import ReactJson from 'react-json-view';
import Editor from '@monaco-editor/react';
import './App.css';

function App() {
  const [code, setCode] = useState('let x = 10;\nlet arr = [1, 2, 3];\nlet obj = { a: 1, b: "hello" };');
  const [tokens, setTokens] = useState(null);
  const [ast, setAst] = useState(null);
  const [logs, setLogs] = useState(null);
  const [error, setError] = useState(null);

  // Layout state (percentages)
  const [leftWidth, setLeftWidth] = useState(40);
  const [topHeight, setTopHeight] = useState(50);
  const [rightTopHeight, setRightTopHeight] = useState(40); // Make AST smaller by default, Console larger
  
  const containerRef = useRef(null);
  const leftPanelRef = useRef(null);
  const rightPanelRef = useRef(null);
  const isResizingHorizontal = useRef(false);
  const isResizingVertical = useRef(false);
  const isResizingRightVertical = useRef(false);

  useEffect(() => {
    const handleMouseMove = (e) => {
      if (isResizingHorizontal.current && containerRef.current) {
        const containerRect = containerRef.current.getBoundingClientRect();
        const newLeftWidth = ((e.clientX - containerRect.left) / containerRect.width) * 100;
        // Limit between 20% and 80%
        if (newLeftWidth > 20 && newLeftWidth < 80) {
          setLeftWidth(newLeftWidth);
        }
      }
      
      if (isResizingVertical.current && leftPanelRef.current) {
        const panelRect = leftPanelRef.current.getBoundingClientRect();
        const newTopHeight = ((e.clientY - panelRect.top) / panelRect.height) * 100;
        // Limit between 20% and 80%
        if (newTopHeight > 20 && newTopHeight < 80) {
          setTopHeight(newTopHeight);
        }
      }

      if (isResizingRightVertical.current && rightPanelRef.current) {
        const panelRect = rightPanelRef.current.getBoundingClientRect();
        const newRightTopHeight = ((e.clientY - panelRect.top) / panelRect.height) * 100;
        // Limit between 20% and 80%
        if (newRightTopHeight > 20 && newRightTopHeight < 80) {
            setRightTopHeight(newRightTopHeight);
        }
      }
    };

    const handleMouseUp = () => {
      isResizingHorizontal.current = false;
      isResizingVertical.current = false;
      isResizingRightVertical.current = false;
      document.body.style.cursor = 'default';
      document.body.style.userSelect = 'auto';
    };

    window.addEventListener('mousemove', handleMouseMove);
    window.addEventListener('mouseup', handleMouseUp);

    return () => {
      window.removeEventListener('mousemove', handleMouseMove);
      window.removeEventListener('mouseup', handleMouseUp);
    };
  }, []);

  const startHorizontalResize = () => {
    isResizingHorizontal.current = true;
    document.body.style.cursor = 'col-resize';
    document.body.style.userSelect = 'none';
  };

  const startVerticalResize = () => {
    isResizingVertical.current = true;
    document.body.style.cursor = 'row-resize';
    document.body.style.userSelect = 'none';
  };

  const startRightVerticalResize = () => {
    isResizingRightVertical.current = true;
    document.body.style.cursor = 'row-resize';
    document.body.style.userSelect = 'none';
  };

  const handleCompile = async () => {
    setError(null);
    setTokens(null);
    setAst(null);
    setLogs(null);
    
    try {
      // Fetch Tokens
      const tokenRes = await fetch('http://localhost:3000/compile', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ code, mode: 'token' }),
      });
      const tokenData = await tokenRes.json();
      if (!tokenRes.ok) throw new Error(tokenData.error || 'Token Compilation failed');
      setTokens(tokenData);

      // Fetch AST
      const astRes = await fetch('http://localhost:3000/compile', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ code, mode: 'ast' }),
      });
      const astData = await astRes.json();
      if (!astRes.ok) throw new Error(astData.error || 'AST Compilation failed');
      setAst(astData);

      // Fetch Execution Logs (Eval)
      const evalRes = await fetch('http://localhost:3000/compile', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ code, mode: 'eval' }),
      });
      const evalData = await evalRes.json();
      if (!evalRes.ok) throw new Error(evalData.error || 'Evaluation failed');
      setLogs(evalData.logs);

    } catch (err) {
      setError(err.message);
    }
  };

  const getTokenClass = (token) => {
    if (typeof token === 'string') return 'token-keyword';
    if (token.Identifier) return 'token-identifier';
    if (token.Number) return 'token-number';
    if (token.String) return 'token-string';
    return 'token-default';
  };

  const formatToken = (token) => {
    if (typeof token === 'string') return token;
    const key = Object.keys(token)[0];
    return `${key}(${token[key]})`;
  };

  return (
    <div className="container">
      <header className="header">
        <h1>JS Compiler Visualizer</h1>
        <button onClick={handleCompile} className="compile-btn">Compile & Run</button>
      </header>

      {error && <div className="error-banner">Error: {error}</div>}

      <div className="main-content" ref={containerRef}>
        
        {/* Left Column */}
        <div 
          className="panel-column" 
          style={{ width: `${leftWidth}%` }}
          ref={leftPanelRef}
        >
          {/* Top: Code Editor */}
          <div className="panel-container" style={{ height: `${topHeight}%` }}>
            <div className="panel-header">Input Code</div>
            <div className="code-editor-wrapper">
              <Editor
                height="100%"
                defaultLanguage="javascript"
                theme="vs-dark"
                value={code}
                onChange={(value) => setCode(value)}
                options={{
                  minimap: { enabled: false },
                  fontSize: 14,
                  wordWrap: 'on',
                  automaticLayout: true,
                  padding: { top: 10 }
                }}
              />
            </div>
          </div>

          <div 
            className="resize-handle-vertical"
            onMouseDown={startVerticalResize}
          />

          {/* Bottom: Lexer Output */}
          <div className="panel-container" style={{ height: `${100 - topHeight}%` }}>
            <div className="panel-header">Lexer Output (Tokens)</div>
            <div className="panel-content tokens-list">
              {tokens ? tokens.map((token, index) => (
                <div key={index} className={`token-item ${getTokenClass(token)}`}>
                  {formatToken(token)}
                </div>
              )) : <span className="placeholder">No tokens generated</span>}
            </div>
          </div>
        </div>

        <div 
          className="resize-handle-horizontal"
          onMouseDown={startHorizontalResize}
        />

        {/* Right Column: AST and Console */}
        <div className="panel-column" style={{ width: `${100 - leftWidth}%` }} ref={rightPanelRef}>
          {/* Top: AST */}
          <div className="panel-container" style={{ height: `${rightTopHeight}%` }}>
             <div className="panel-header">AST</div>
             <div className="panel-content ast-view">
               {ast ? (
                 <ReactJson 
                   src={ast} 
                   theme="monokai" 
                   displayDataTypes={false} 
                   displayObjectSize={false} 
                   enableClipboard={true} 
                   style={{ backgroundColor: 'transparent' }}
                 />
               ) : (
                 <span className="placeholder">No AST generated</span>
               )}
             </div>
          </div>

          <div 
            className="resize-handle-vertical"
            onMouseDown={startRightVerticalResize}
          />

          {/* Bottom: Console Output */}
          <div className="panel-container" style={{ height: `${100 - rightTopHeight}%` }}>
            <div className="panel-header">Console Output</div>
            <div className="panel-content console-view">
              {logs ? (
                <pre>{logs}</pre>
              ) : (
                <span className="placeholder">No output</span>
              )}
            </div>
          </div>
        </div>

      </div>
    </div>
  );
}

export default App;
