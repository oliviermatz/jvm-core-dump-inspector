/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 */

package org.netbeans.lib.profiler.heap;


/**
 *
 * @author Tomas Hurka
 */
public class StackFrame extends HprofObject {
    
    static final int NO_LINE_INFO = 0;
    static final int UNKNOWN_LOCATION = -1;
    static final int COMPILED_METHOD = -2;
    static final int NATIVE_METHOD = -3;
    
    //~ Instance fields ----------------------------------------------------------------------------------------------------------
    
    private final StackFrameSegment stackFrameSegment;
    
    //~ Constructors -------------------------------------------------------------------------------------------------------------
    
    StackFrame(StackFrameSegment segment, long offset) {
        super(offset);
        stackFrameSegment = segment;
        assert getHprofBuffer().get(offset) == HprofHeap.STACK_FRAME;
    }
    
    //~ Methods ------------------------------------------------------------------------------------------------------------------
    
    long getStackFrameID() {
        return getHprofBuffer().getID(fileOffset + stackFrameSegment.stackFrameIDOffset);
    }
    
    String getMethodName() {
        return getStringByOffset(stackFrameSegment.methodIDOffset);
    }
    
    String getMethodSignature() {
        return getStringByOffset(stackFrameSegment.methodSignatureIDOffset);
    }
    
    String getSourceFile() {
        return getStringByOffset(stackFrameSegment.sourceIDOffset);
    }
    
    String getClassName() {
        int classSerial = getHprofBuffer().getInt(fileOffset + stackFrameSegment.classSerialNumberOffset);
        return stackFrameSegment.getClassNameBySerialNumber(classSerial);
    }
    
    int getLineNumber() {
        return getHprofBuffer().getInt(fileOffset + stackFrameSegment.lineNumberOffset);
    }
    
    private HprofByteBuffer getHprofBuffer() {
        return stackFrameSegment.hprofHeap.dumpBuffer;
    }
    
    private String getStringByOffset(long offset) {
        long stringID = getHprofBuffer().getID(fileOffset + offset);
        return stackFrameSegment.hprofHeap.getStringSegment().getStringByID(stringID);
    }
}
