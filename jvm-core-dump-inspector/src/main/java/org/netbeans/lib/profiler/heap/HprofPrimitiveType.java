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

import java.util.HashMap;
import java.util.Map;


/**
 *
 * @author Tomas Hurka
 */
public class HprofPrimitiveType implements PrimitiveType {
    //~ Static fields/initializers -----------------------------------------------------------------------------------------------

    private static Map primitiveTypeMap;

    static {
        primitiveTypeMap = new HashMap(10);
        primitiveTypeMap.put(Integer.valueOf(HprofHeap.BOOLEAN), new HprofPrimitiveType("boolean")); //NOI18N
        primitiveTypeMap.put(Integer.valueOf(HprofHeap.CHAR), new HprofPrimitiveType("char")); //NOI18N
        primitiveTypeMap.put(Integer.valueOf(HprofHeap.FLOAT), new HprofPrimitiveType("float")); //NOI18N
        primitiveTypeMap.put(Integer.valueOf(HprofHeap.DOUBLE), new HprofPrimitiveType("double")); //NOI18N
        primitiveTypeMap.put(Integer.valueOf(HprofHeap.BYTE), new HprofPrimitiveType("byte")); //NOI18N
        primitiveTypeMap.put(Integer.valueOf(HprofHeap.SHORT), new HprofPrimitiveType("short")); //NOI18N
        primitiveTypeMap.put(Integer.valueOf(HprofHeap.INT), new HprofPrimitiveType("int")); //NOI18N
        primitiveTypeMap.put(Integer.valueOf(HprofHeap.LONG), new HprofPrimitiveType("long")); //NOI18N
    }

    //~ Instance fields ----------------------------------------------------------------------------------------------------------

    private String typeName;

    //~ Constructors -------------------------------------------------------------------------------------------------------------

    private HprofPrimitiveType(String name) {
        typeName = name;
    }

    //~ Methods ------------------------------------------------------------------------------------------------------------------

    public String getName() {
        return typeName;
    }

    static Type getType(byte type) {
        return (Type) primitiveTypeMap.get(Integer.valueOf(type));
    }
}
